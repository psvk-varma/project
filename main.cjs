@compiler >= 6

include "String.aes"

contract DoctorAppointment =

  record state = {
    doctor: address,
    patients: map(int, patient) }
  
  record patient = {
    firstName: string,
    lastName:string,
    age: int,
    gender:string,
    time: string,
    price: int,
    ispaid: bool,
    isvisited: bool,
    problem_Detected:string }

  entrypoint init() : state =
    { doctor = Call.caller, patients = {} }

  stateful entrypoint add_patient(firstName: string,lastName:string,age:int,gender:string) : int =
    let patient_id = Map.size(state.patients) + 1
    let new_patient = {
      firstName = firstName,
      lastName = lastName, 
      age = age,
      gender =gender,
      time = "Null",
      price = 0,
      ispaid=false,
      isvisited=false,
      problem_Detected="Not diagnosed till now"}

    put(state{ patients[patient_id] = new_patient })
    patient_id

  stateful entrypoint patient_visited(idNumber:int,price:int, problem_Detected:string) =
    require(Call.caller == state.doctor ,"You are not authorized to perform this task")
    let getInfo = 
        switch(Map.lookup(idNumber,state.patients))
            None    => abort("No Patient with that id")
            Some(e) => e

    let updateInfo = getInfo{price = price,problem_Detected=problem_Detected,isvisited=true}
    put(state{patients[idNumber] = updateInfo})

  entrypoint get_patient(patient_id: int) : option(patient) =
    Map.lookup(patient_id, state.patients)
