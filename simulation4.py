# Blair Bilodeau
# Internal Medicine Simulation Tool
# team assigned by lowest census, take first available bed with full swapping


## Libraries

PATH = '/Users/blairbilodeau/Documents/Research/NSERC_USRA/Simulation_Model/Model7/'
import numpy as np
import pandas as pd
import operator
import os.path
import sys
sys.path.append(PATH)
from Events import Events as Events

## Column names

PATIENT_COLS = ['Origin', 'Status', 'Acuity', 'Initial_Team', 'Floor_Team', 'Bed', 'Admit_Time', 'Off-Service_Time', 'Off-Service_Location', 'Decant_Time', 'Floor_Time', 'Discharge_Order', 'Discharged'] # data to track for each patient
NUM_PATIENT_COLS = len(PATIENT_COLS)
BED_COLS = ['Nurse', 'Patient', 'Team', 'Condition', 'Status', 'Acuity'] # data to track for each bed
NUM_BED_COLS = len(BED_COLS)
CENSUS_COLS = ['Time', 'Team1_Emerg', 'Team2_Emerg', 'Team3_Emerg', 'Team1_Decant', 'Team2_Decant', 'Team3_Decant', 'Team1_Off-Service', 'Team2_Off-Service', 'Team3_Off-Service', 'Team1_Floor', 'Team2_Floor', 'Team3_Floor', 'Non-Medicine_Floor'] # summarized data to track
NUM_CENSUS_COLS = len(CENSUS_COLS)
NURSE_COLS = ['Time', 'Team1_Patients', 'Team2_Patients', 'Team3_Patients', 'Team1_Nurses', 'Team2_Nurses', 'Team3_Nurses']
NUM_NURSE_COLS = len(NURSE_COLS)
TIMING_DF = pd.read_csv(PATH + 'Timing.csv', index_col=0) # table of time (in seconds) to move from one location to another

#############################################################################################################################################################################

## Time dependent sampling distributions

DECANT_OPEN = 7 # time of day that patients can start being admitted to decant
DECANT_CLOSE = 19 # time of day that patients stop being admitted to decant
NUM_OFF_SERVICE = 1

# time at which next admittal will occur
def ADMIT_TIME(): 
    hour = int((sim_time % 24) // 4)
    rates = [1*i for i in [2.1, 4.2, 1.6, 1.2, 1.2, 1.5]]
    time = np.random.exponential(rates[hour])
    return(sim_time + time)

# time at which patient will be done treatment and a discharge order is made
def TREAT_TIME():
    prob = 1/5.3

    day = ((sim_time // 24) % 364) + np.random.geometric(prob)
    year = (sim_time // 24) // 364

    wknd_prob = 0.7
    if np.random.binomial(1,wknd_prob)==0:
        # check for Saturday
        if day%7 == 5:
            day += 2
        # check for Sunday
        elif day%7 == 6:
            day += 1
    hour_prob =[0, 0, 0, 0, 0, 0.000925, 0.000925, 0.0297, 0.0483, 0.0781, 0.1747, 0.2342, 0.1041, 0.1227, 0.0892, 0.0446, 0.0372, 0.0112, 0.0149, 0.0037, 0.002775, 0.000925, 0.000925, 0.000925]
    hour = np.where(np.random.multinomial(1,hour_prob)==1)[0][0]
    return(year*8736 + day*24 + hour)
    
# time at which patient will be discharged from bed
def DISCHARGE_TIME(temp_sim_time):
    hour = temp_sim_time % 24
    if hour < 11:
        time = np.random.uniform(1,3.25)
    elif hour < 14:
        time = np.random.uniform(2,3.5)
    else:
        time = np.random.uniform(0, 1)
    return(temp_sim_time + time)

# time at which the bed will be clean
def CLEAN_TIME(Contact):
    if Contact:
        return(sim_time + 2)
    else:
        return(sim_time + 1)
    
# source of the admitted patient
def ORIGIN():
    prob = [0.81, 0.046, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.137]
    index = np.where(np.random.multinomial(1,prob)==1)[0][0] 
    return(ORIGINS[index])

# type of room that the patient will require
def STATUS():
    prob = [0.2, 0.25, 0.55]
    index = np.where(np.random.multinomial(1,prob)==1)[0][0]
    return(STATUSES[index])

def ACUITY():
    prob = [0.05, 0.6, 0.35]
    index = np.where(np.random.multinomial(1,prob)==1)[0][0]
    return(ACUITIES[index])

# team assigned to the patient
def TEAM():
    team_census = [len(current_patients[team]) for team in [1,2,3]]
    return(np.random.choice([team for team in [1,2,3] if team_census[team-1]==min(team_census)]))
    
# order the teams from lowest census to highest
def RANKED_CENSUS(census):
    if census[2] < census[1] or (census[2] == census[1] and np.random.binomial(1,0.5)==1):
        if census[2] < census[0] or (census[2] == census[0] and np.random.binomial(1,0.5)==1):
            if census[1] < census[0] or (census[1] == census[0] and np.random.binomial(1,0.5)==1):
                return([3,2,1])
            else:
                return([3,1,2])
        else:
            return([1,3,2])
    else:
        if census[1] < census[0] or (census[1] == census[0] and np.random.binomial(1,0.5)==1):
            if census[2] < census[0] or (census[2] == census[0] and np.random.binomial(1,0.5)==1):
                return([2,3,1])
            else:
                return([2,1,3])
        else:
            return([1,2,3]) 
        
# Off-Service
OFF_SERVICE = ['FLOOR4', 'FLOOR5', 'FLOOR6', 'FLOOR7', 'FLOOR8', 'FLOOR9', 'FLOOR10']

# Origins
ORIGINS = ['ED', 'ICU'] + OFF_SERVICE + ['NON-MEDICINE']

# Statuses
STATUSES = ['PRIVATE', 'SEMI', 'WARD']

#Acuities
ACUITIES = ['HIGH', 'MEDIUM', 'LOW']

# Bed Priorities
PRIORITIES = {'ED':1, 'ICU':1, 'DECANT':2, 'FLOOR4':3, 'FLOOR5':3, 'FLOOR6':3, 'FLOOR7':3, 'FLOOR8':3, 'FLOOR9':3, 'FLOOR10':3, 'NON-MEDICINE':4}

## Event Types
#ADMIT 
#DISCHARGE 
#CLEAN 
#NURSE
#SUMMARIZE 

## Bed Conditions
#AVAILABLE 
#OCCUPIED 
#DIRTY

##############################################################################################################################################

## Simulation Functions

# The ward gets a request to place a patient into a bed
def simAdmit(patient_ID):
    global patient_num

    if not REPLICATE:
        patient_df.loc[patient_ID] = [None for i in range(NUM_PATIENT_COLS)]
        patient_df.loc[patient_ID, ['Origin', 'Admit_Time', 'Discharge_Order', 'Status', 'Acuity']] = [ORIGIN(), sim_time, TREAT_TIME(), STATUS(), ACUITY()]
        events.enqueue('DISCHARGE', DISCHARGE_TIME(patient_df.loc[patient_ID, 'Discharge_Order']), patient_ID)
        patient_num += 1
        events.enqueue('ADMIT', ADMIT_TIME(), patient_num)
    else:
        if np.isfinite(patient_df.loc[patient_ID, 'Discharged']):
            events.enqueue('DISCHARGE', patient_df.loc[patient_ID, 'Discharged'], patient_ID)
        else:
            events.enqueue('DISCHARGE', WARMUP+SIMULATION + 1, patient_ID)
        patient_df.loc[patient_ID, 'Discharged'] = None
        
    patient_origin, patient_status, patient_acuity = patient_df.loc[patient_ID, ['Origin', 'Status', 'Acuity']]
        
    patient_df.loc[patient_ID, 'Bed'] = patient_origin
        
    if patient_status == 'PRIVATE':
        possible_beds = bed_df[np.array(bed_df.Condition=='AVAILABLE') & np.array(bed_df.Status=='PRIVATE') & np.array([bed[0]=='4' and bed_df.loc[bed, 'Nurse'][0:2]=='RN' if patient_acuity=='HIGH' else True for bed in bed_df.index])]
        if patient_origin == 'NON-MEDICINE':
            patient_df.loc[patient_ID, ['Initial_Team', 'Floor_Team']] = [0, 0]
            if len(possible_beds) > 0:
                simTransfer(patient_ID, np.random.choice(possible_beds.index))
        else:
            teams_ranked = RANKED_CENSUS([len(current_patients[team]) for team in [1,2,3]])
            bed_found = False
            for team in teams_ranked:
                beds = possible_beds[possible_beds.Team==team].index
                if len(beds) > 0:
                    bed_found = True
                    patient_df.loc[patient_ID, ['Initial_Team', 'Floor_Team']] = [team, team]
                    simTransfer(patient_ID, np.random.choice(beds))
                    break
            if not bed_found:
                patient_df.loc[patient_ID, 'Initial_Team'] = TEAM()
                             
    elif patient_status == 'SEMI':
        possible_beds = bed_df[np.array(bed_df.Condition=='AVAILABLE') & np.array([status in ['PRIVATE', 'SEMI'] for status in bed_df.Status]) & np.array([bed[0]=='4' and bed_df.loc[bed, 'Nurse'][0:2]=='RN' if patient_acuity=='HIGH' else True for bed in bed_df.index])]
        if patient_origin == 'NON-MEDICINE':
            patient_df.loc[patient_ID, ['Initial_Team', 'Floor_Team']] = [0, 0]
            if len(possible_beds) > 0:
                simTransfer(patient_ID, np.random.choice(possible_beds.index))
        else:
            teams_ranked = RANKED_CENSUS([len(current_patients[team]) for team in [1,2,3]])
            bed_found = False
            for team in teams_ranked:
                beds = possible_beds[possible_beds.Team==team].index
                if len(beds) > 0:
                    bed_found = True
                    patient_df.loc[patient_ID, ['Initial_Team', 'Floor_Team']] = [team, team]
                    simTransfer(patient_ID, np.random.choice(beds))
                    break
            if not bed_found:
                patient_df.loc[patient_ID, 'Initial_Team'] = TEAM()
  
    elif patient_status == 'WARD':
        possible_beds = bed_df[np.array(bed_df.Condition=='AVAILABLE') & np.array(bed_df.Status!='DECANT') & np.array([bed[0]=='4' and bed_df.loc[bed, 'Nurse'][0:2]=='RN' if patient_acuity=='HIGH' else True for bed in bed_df.index])]
        if patient_origin == 'NON-MEDICINE':
            patient_df.loc[patient_ID, ['Initial_Team', 'Floor_Team']] = [0, 0]
            if len(possible_beds) > 0:
                simTransfer(patient_ID, np.random.choice(possible_beds.index))
        else:
            teams_ranked = RANKED_CENSUS([len(current_patients[team]) for team in [1,2,3]])
            bed_found = False
            for team in teams_ranked:
                beds = possible_beds[possible_beds.Team==team].index
                if len(beds) > 0:
                    bed_found = True
                    patient_df.loc[patient_ID, ['Initial_Team', 'Floor_Team']] = [team, team]
                    simTransfer(patient_ID, np.random.choice(beds))
                    break
            if not bed_found:
                patient_df.loc[patient_ID, 'Initial_Team'] = TEAM()
                if patient_origin in ['ED', 'ICU']:
                    decant_beds = bed_df[np.array(bed_df.Condition=='AVAILABLE') & np.array(bed_df.Status=='DECANT')].index
                    if len(decant_beds) > 0 and sim_time%24 < DECANT_CLOSE and sim_time%24 > DECANT_OPEN:
                        simDecant(patient_ID, np.random.choice(decant_beds))
                    elif len(off_service_beds) < NUM_OFF_SERVICE:
                        simOffService(patient_ID)                

    current_patients[patient_df.loc[patient_ID, 'Initial_Team']].append(patient_ID)
    
# move a patient into a decant bed
def simDecant(patient_ID, bed_ID):                
    bed_df.loc[bed_ID, ['Condition', 'Patient', 'Team']] = ['OCCUPIED', patient_ID, patient_df.loc[patient_ID, 'Initial_Team']]
    patient_df.loc[patient_ID, ['Decant_Time', 'Bed']] = [sim_time, bed_ID]

# put someone in an off-service bed
def simOffService(patient_ID):
    patient_df.loc[patient_ID, ['Off-Service_Time', 'Off-Service_Location']] = [sim_time, np.random.choice(OFF_SERVICE)]
    off_service_beds.append(patient_ID)

# move a patient into a ward bed
def simTransfer(patient_ID, bed_ID):
    
    if patient_df.loc[patient_ID, 'Decant_Time'] != None:
        decant_bed = patient_df.loc[patient_ID, 'Bed']
        bed_df.loc[decant_bed, ['Patient', 'Team', 'Condition']] = [None, None, 'DIRTY']
        events.enqueue('CLEAN', CLEAN_TIME(False), decant_bed)
        
    elif patient_df.loc[patient_ID, 'Off-Service_Time'] != None:
        off_service_beds.remove(patient_ID)         
    
    bed_df.loc[bed_ID, ['Condition', 'Patient']] = ['OCCUPIED', patient_ID]
    patient_df.loc[patient_ID, ['Floor_Time', 'Bed']] = [sim_time, bed_ID]
    if patient_df.loc[patient_ID, 'Origin'] != 'NON-MEDICINE' and patient_df.loc[patient_ID, 'Admit_Time'] != sim_time:
        patient_df.loc[patient_ID, 'Floor_Team'] = bed_df.loc[bed_ID, 'Team']
        current_patients[patient_df.loc[patient_ID, 'Initial_Team']].remove(patient_ID)
        current_patients[patient_df.loc[patient_ID, 'Floor_Team']].append(patient_ID)
 
# discharge a patient        
def simDischarge(patient_ID):
    
    if patient_df.loc[patient_ID, 'Floor_Time'] != None:
        bed_df.loc[patient_df.loc[patient_ID, 'Bed'], ['Patient', 'Condition']] = [None, 'DIRTY']
        if patient_df.loc[patient_ID, 'Status'] == 'PRIVATE':
            events.enqueue('CLEAN', CLEAN_TIME(True), patient_df.loc[patient_ID, 'Bed'])
        else:
            events.enqueue('CLEAN', CLEAN_TIME(False), patient_df.loc[patient_ID, 'Bed'])
        
    elif patient_df.loc[patient_ID, 'Decant_Time'] != None:
        bed_df.loc[patient_df.loc[patient_ID, 'Bed'], ['Patient', 'Team', 'Condition']] = [None, None, 'DIRTY']
        patient_df.loc[patient_ID, 'Floor_Team'] = patient_df.loc[patient_ID, 'Initial_Team']
        events.enqueue('CLEAN', CLEAN_TIME(False), patient_df.loc[patient_ID, 'Bed']) 
        patient_df.loc[patient_ID, 'Floor_Time'] = sim_time
        
    elif patient_df.loc[patient_ID, 'Off-Service_Time'] != None:
        off_service_beds.remove(patient_ID)
        patient_df.loc[patient_ID, 'Floor_Team'] = patient_df.loc[patient_ID, 'Initial_Team']
        patient_df.loc[patient_ID, 'Floor_Time'] = sim_time
        
    else:
        patient_df.loc[patient_ID, 'Floor_Team'] = patient_df.loc[patient_ID, 'Initial_Team']
        patient_df.loc[patient_ID, 'Floor_Time'] = sim_time

    patient_df.loc[patient_ID, 'Discharged'] = sim_time
    current_patients[patient_df.loc[patient_ID, 'Floor_Team']].remove(patient_ID)
    
# clean a ward or decant bed        
def simClean(bed_ID):    
    
    bed_df.loc[bed_ID, 'Condition'] = 'AVAILABLE'
    bed_status, bed_nurse = bed_df.loc[bed_ID, ['Status', 'Nurse']]
    possible_patients = patient_df.loc[[patient for team in list(current_patients.values()) for patient in team]]            
    if bed_ID[0]=='6' or np.random.binomial(1, 1/3)==1:
        consider_decant = True
    else:
        consider_decant = False
    
    if bed_status == 'PRIVATE':
        qualified_patients = possible_patients[np.array(possible_patients.Floor_Time.isnull()) & np.array([patient_df.loc[patient, 'Decant_Time']==None if not consider_decant else True for patient in possible_patients.index]) & np.array([patient_df.loc[patient, 'Acuity']!='HIGH' if 'P' in bed_nurse or bed_ID[0]=='6' else True for patient in possible_patients.index])].index        
        if len(qualified_patients) > 0:
            simTransfer(prioritySort(qualified_patients), bed_ID) 
            
    elif bed_status == 'SEMI':
        qualified_patients = possible_patients[np.array(possible_patients.Floor_Time.isnull()) & np.array([patient_df.loc[patient, 'Decant_Time']==None if not consider_decant else True for patient in possible_patients.index]) & np.array(possible_patients.Status!='PRIVATE') & np.array([patient_df.loc[patient, 'Acuity']!='HIGH' if 'P' in bed_nurse or bed_ID[0]=='6' else True for patient in possible_patients.index])].index        
        if len(qualified_patients) > 0:
            simTransfer(prioritySort(qualified_patients), bed_ID) 

    elif bed_status == 'WARD':
        qualified_patients = possible_patients[np.array(possible_patients.Floor_Time.isnull()) & np.array([patient_df.loc[patient, 'Decant_Time']==None if not consider_decant else True for patient in possible_patients.index]) & np.array(possible_patients.Status=='WARD') & np.array([patient_df.loc[patient, 'Acuity']!='HIGH' if 'P' in bed_nurse or bed_ID[0]=='6' else True for patient in possible_patients.index])].index        
        if len(qualified_patients) > 0:
            simTransfer(prioritySort(qualified_patients), bed_ID)        
    
    elif bed_status == 'DECANT':
        qualified_patients = possible_patients[np.array(possible_patients.Floor_Time.isnull()) & np.array(possible_patients.Decant_Time.isnull()) & np.array(possible_patients['Off-Service_Time'].isnull()) & np.array(possible_patients.Status=='WARD')].index        
        if len(qualified_patients) > 0:
            simDecant(prioritySort(qualified_patients), bed_ID) 
  
# assign the beds to each nurse
def simNurse(time_of_day):
    if time_of_day == 'NIGHT':
        opp_time_of_day = 'DAY'
        num_rn = 9
        num_rpn = 1
        num_decant = 0
    else:
        opp_time_of_day = 'NIGHT'
        num_rn = 13
        num_rpn = 2
        if len(bed_df[np.array(bed_df.Status=='DECANT') & np.array(bed_df.Patient.notnull())]) > 4:
            num_decant = 2
        else:
            num_decant = 1
    num_6 = 3
    
    team_patients = [sum([patient_df.loc[patient, 'Floor_Time']!=None for patient in current_patients[team]]) for team in [1,2,3]]      
    team_nurses = [len(set(bed_df[bed_df.Team==team].Nurse)) for team in [1,2,3]]
    nurse_df.loc[sim_time-0.01] = ['END ' + opp_time_of_day] + team_patients + team_nurses
    
    
    if sim_time > 0:
        events.enqueue('NURSE', sim_time+24, time_of_day)
    
    bed_df.Nurse = [None for i in range(len(bed_df))]    
    
    # start with fourth floor
    beds_per_nurse = int(60 / (num_rn + num_rpn))
    beds = list(bed_df.loc[[bed[0]=='4' for bed in bed_df.index]].index)
    
    # first assign the RPN to beds without high acuity patients
    rpn_beds = [bed for bed in beds if bed_df.loc[bed,'Patient']==None or patient_df.loc[bed_df.loc[bed,'Patient'], 'Acuity']!='HIGH']
    for i in range(num_rpn):
        nurse = 'RPN' + str(i+1)
        if len(rpn_beds) < beds_per_nurse:
            print('OOPS')
            sys.exit()
        if i%2==0:
            init_bed = min(rpn_beds)
        else:
            init_bed = max(rpn_beds)
        rpn_beds.remove(init_bed)
        bed_distances = {bed : TIMING_DF.loc[init_bed, bed] for bed in rpn_beds}
        nurse_beds = [pair[0] for pair in sorted(bed_distances.items(), key=operator.itemgetter(1))[0:beds_per_nurse-1]]
        for bed in nurse_beds:
            rpn_beds.remove(bed)        
        bed_df.loc[nurse_beds + [init_bed], 'Nurse'] = [nurse for i in range(len(nurse_beds)+1)]
        
    # now assign remaining fourth floor beds to RN
    rn_beds = [bed for bed in beds if bed_df.loc[bed,'Nurse']==None]
    for j in range(num_rn):
        nurse = 'RN' + str(j+1)
        init_bed = min(rn_beds)
        rn_beds.remove(init_bed)
        bed_distances = {bed : TIMING_DF.loc[init_bed, bed] for bed in rn_beds}
        nurse_beds = [pair[0] for pair in sorted(bed_distances.items(), key=operator.itemgetter(1))[0:beds_per_nurse-1]]
        for bed in nurse_beds:
            rn_beds.remove(bed)
        bed_df.loc[nurse_beds + [init_bed], 'Nurse'] = [nurse for i in range(len(nurse_beds)+1)]        
    
    if time_of_day == 'DAY':
        # now assign sixth floor beds
        beds_6 = list(bed_df.loc[np.array([bed[0]=='6' for bed in bed_df.index]) & np.array(bed_df.Status!='DECANT')].index)
        beds_per_nurse = 4
        
        for k in range(num_6):
            nurse = '6RN' + str(k+1)
            init_bed = min(beds_6)
            beds_6.remove(init_bed)
            bed_distances = {bed : TIMING_DF.loc[init_bed, bed] for bed in beds_6}
            nurse_beds = [pair[0] for pair in sorted(bed_distances.items(), key=operator.itemgetter(1))[0:beds_per_nurse-1]]
            for bed in nurse_beds:
                beds_6.remove(bed)
            bed_df.loc[nurse_beds + [init_bed], 'Nurse'] = [nurse for i in range(len(nurse_beds)+1)]      
            
        # now assign decant beds
        decant_beds = list(bed_df.loc[bed_df.Status=='DECANT'].index)
        beds_per_nurse = int(6 / num_decant)
        
        for l in range(num_decant):
            nurse = 'DRN' + str(l+1)
            init_bed = min(decant_beds)
            decant_beds.remove(init_bed)
            bed_distances = {bed : TIMING_DF.loc[init_bed, bed] for bed in decant_beds}
            nurse_beds = [pair[0] for pair in sorted(bed_distances.items(), key=operator.itemgetter(1))[0:beds_per_nurse-1]]
            for bed in nurse_beds:
                decant_beds.remove(bed)
            bed_df.loc[nurse_beds + [init_bed], 'Nurse'] = [nurse for i in range(len(nurse_beds)+1)] 
        
    elif time_of_day == 'NIGHT':
        # now assign sixth floor beds
        beds_6 = list(bed_df.loc[[bed[0]=='6' for bed in bed_df.index]].index)
        beds_per_nurse = 6
        
        for k in range(num_6):
            nurse = '6RN' + str(k+1)
            init_bed = min(beds_6)
            beds_6.remove(init_bed)
            bed_distances = {bed : TIMING_DF.loc[init_bed, bed] for bed in beds_6}
            nurse_beds = [pair[0] for pair in sorted(bed_distances.items(), key=operator.itemgetter(1))[0:beds_per_nurse-1]]
            for bed in nurse_beds:
                beds_6.remove(bed)
            bed_df.loc[nurse_beds + [init_bed], 'Nurse'] = [nurse for i in range(len(nurse_beds)+1)]
       
    
    team_patients = [sum([patient_df.loc[patient, 'Floor_Time']!=None for patient in current_patients[team]]) for team in [1,2,3]]      
    team_nurses = [len(set(bed_df[bed_df.Team==team].Nurse)) for team in [1,2,3]]
    nurse_df.loc[sim_time+0.01] = ['START ' + time_of_day] + team_patients + team_nurses
    
# summarize the census data for a day in the simulation    
def summarize(time_of_day):

    medicine_patients = patient_df.loc[current_patients[1] + current_patients[2] + current_patients[3]]
    non_medicine_patients = patient_df.loc[current_patients[0]]
    floor_patients = medicine_patients[medicine_patients.Floor_Time.notnull()]
    emerg_patients = medicine_patients[medicine_patients.Bed=='ED']
    decant_patients = medicine_patients[np.array(medicine_patients.Floor_Time.isnull()) & np.array(medicine_patients.Decant_Time.notnull())]
    off_service_patients = medicine_patients[np.array(medicine_patients.Floor_Time.isnull()) & (np.array(medicine_patients.Origin!='ED') | np.array(medicine_patients['Off-Service_Time'].notnull()))]
    census_df.loc[sim_time] = [time_of_day] + [sum(emerg_patients.Initial_Team==team) for team in [1,2,3]] + [sum(decant_patients.Initial_Team==team) for team in [1,2,3]] + [sum(off_service_patients.Initial_Team==team) for team in [1,2,3]] + [sum(floor_patients.Floor_Team==team) for team in [1,2,3]] + [sum(non_medicine_patients.Floor_Time.notnull())] 
    events.enqueue('SUMMARIZE', sim_time + 24, time_of_day) 
    
# from list of patients determine who should move into the available bed    
def prioritySort(patient_list):
    num_patients = len(patient_list)
    curr_index = num_patients-1
    curr_admit, curr_bed = patient_df.loc[patient_list[curr_index], ['Admit_Time', 'Bed']]
    if curr_bed[0] == '6':
        curr_priority = PRIORITIES['DECANT']
    else:
        curr_priority = PRIORITIES[curr_bed]
    prev_index = num_patients-1
    if num_patients > 1:
        while prev_index > 0:
            prev_index -= 1
            prev_admit, prev_bed = patient_df.loc[patient_list[prev_index], ['Admit_Time', 'Bed']]
            if prev_bed[0] == '6':
                prev_priority = PRIORITIES['DECANT']
            else:
                prev_priority = PRIORITIES[prev_bed]
            if prev_priority < curr_priority or (prev_priority == curr_priority and prev_admit < curr_admit):
                curr_index = prev_index
                curr_admit = prev_admit
                curr_priority = prev_priority
    return(patient_list[curr_index])
   
##############################################################################################################################################
    
### Run Simulation 

WARMUP = 4368 # number of hours until simulation starts
SIMULATION = 8736 # number of hours of simulation
VERSION = 1 # parameter version
WARD = 1
NUM_SIMS = 20 # number of simulations to run
REPLICATE = True

for run in range(1, NUM_SIMS+1):
    
    ## Initialization
    
    # State structures
    off_service_beds = []
    events = Events() 
    patient_df = pd.DataFrame(columns=PATIENT_COLS)
    if REPLICATE:
        patients = 'Sim0/patient_df_v' + str(VERSION) + 'r' + str(run) + '.csv'
        old_patient_df = pd.read_csv(PATH + patients, header=0)
        old_patient_df = old_patient_df.drop(old_patient_df.columns[0], axis=1)
        for row in range(len(old_patient_df)):
            old_patient = old_patient_df.loc[row]
            patient_ID = row+1
            patient_df.loc[patient_ID] = [None for i in range(NUM_PATIENT_COLS)]
            patient_df.loc[patient_ID, ['Admit_Time', 'Acuity', 'Status', 'Origin', 'Discharge_Order', 'Discharged']] = old_patient_df.loc[row, ['Admit_Time', 'Acuity', 'Status', 'Origin', 'Discharge_Order', 'Discharged']]
            events.enqueue('ADMIT', patient_df.loc[patient_ID, 'Admit_Time'], patient_ID) 
    
    bed_df = pd.DataFrame(columns=BED_COLS)
    nurse_df = pd.DataFrame(columns=NURSE_COLS)
    census_df = pd.DataFrame(columns=CENSUS_COLS)
    census_df.loc[0] = [0 for i in range(NUM_CENSUS_COLS)]
    current_patients = {0:[], 1:[], 2:[], 3:[]}
    
    # State variables
    sim_time = 0 # number of minutes since simulation began
    patient_num = 1 # number of patients who have entered the ED since the sim started 

    # Populate bed data
    temp = pd.read_csv(PATH + 'Ward' + str(WARD) + '.csv')
    for row in range(len(temp)):
        record = temp.loc[row]
        bed_df.loc[record.ID] = [None for i in range(NUM_BED_COLS)]
        bed_df.loc[record.ID, ['Condition', 'Status', 'Team', 'Acuity']] = ['AVAILABLE', record.Status, record.Team, record.Acuity]
    
    ## Simulation
    
    if not REPLICATE:
        events.enqueue('ADMIT', ADMIT_TIME(), patient_num)
    simNurse('NIGHT')
    events.enqueue('NURSE', 7, 'DAY')
    events.enqueue('NURSE', 19, 'NIGHT')
    events.enqueue('SUMMARIZE', 7, 'MORNING')
    events.enqueue('SUMMARIZE', 17, 'EVENING')
    events.enqueue('SUMMARIZE', 24, 'MIDNIGHT')
    while events.size() > 0 and events.peek() <= WARMUP + SIMULATION:
        
        event = events.dequeue()
        event_type = event[0]
        sim_time = event[1]
        event_ID = event[2]

        if event_type == 'ADMIT':
            simAdmit(event_ID)
        
        elif event_type == 'DISCHARGE':
            simDischarge(event_ID)
            
        elif event_type == 'CLEAN':
            simClean(event_ID)
                
        elif event_type == 'SUMMARIZE':
            summarize(event_ID)
        
        elif event_type == 'NURSE':
            simNurse(event_ID)

        if sum(bed_df.Nurse.isnull()) > 0:
            print('GONE')
            sys.exit()
    
    # Output tables to csv files while avoiding overwriting 
    end = 'v' + str(VERSION) + 'w' + str(WARD) + 'r' + str(run) + '.csv'
    
    file1 = PATH + 'Sim4/patient_df_' + end
    if os.path.isfile(file1):
        print('FILE ALREADY EXISTS: ' + file1)
        sys.exit()
    else:
        patient_df.to_csv(file1)
        
    file2 = PATH + 'Sim4/census_df_' + end
    if os.path.isfile(file2):
        print('FILE ALREADY EXISTS: ' + file2)
        sys.exit()
    else:
        census_df.to_csv(file2)     
        
    file3 = PATH + 'Sim4/nurse_df_' + end
    if os.path.isfile(file3):
        print('FILE ALREADY EXISTS: ' + file3)
        sys.exit()
    else:
        nurse_df.to_csv(file3)         

##############################################################################################################################################

