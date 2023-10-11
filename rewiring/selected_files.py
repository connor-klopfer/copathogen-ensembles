"""
    Specific files from the missing data analysis. Prevents analysis of tables that have already been 
    done/not needed. 

    Updated as of 05Feb21
"""

specific_files = [
    # Simple Representation 
    # "EXP_ac8498c6-e73a-4f72-83f0-37e10c479aee.csv",
    # "EXP_237fb9ed-f2d3-4fa1-9572-7b2b2855ddd9.csv",
    # "EXP_952a7a2c-2c46-49ba-a6cb-cd856d4b175b.csv",
    # "EXP_1303b544-9078-444f-b9ad-8be4b0ce0ece.csv",
    # "EXP_da5bad7b-d6a4-4069-bd3f-ee7fdf97cc75.csv",
    # "EXP_2a362919-b45c-4d78-85a2-6e9be7b7c05b.csv",
    # "EXP_2bbb2efc-549d-431b-975f-80187bedb846.csv",
    # "EXP_3270b1fb-1306-439e-bacf-08e3b3c938a2.csv", 
  # "EXP_9a3db97c-9fc3-4a9f-a008-69bfef14a28e.csv", 
  # "EXP_56024e1f-ee55-474c-bf7d-02f09c7444ac.csv",
  # "EXP_9e248642-af26-4e39-a7a1-1364cc17dc48.csv",
  # "EXP_8776d00e-daa8-4b66-950c-a7c3d2b5c090.csv", 
  # "EXP_f4f9545e-8f00-4f62-ab25-6719ceea2bd5.csv",
  # "EXP_0e6d45e5-772a-4cdb-812c-8059c90ab961.csv", 
  # "EXP_77ce5953-7eca-4a88-9681-9c8a8eaf65ba.csv", 
  # "EXP_4ee43b11-695d-442e-a4ef-e63e8f421973.csv",
  # "EXP_615a62db-aeca-4b7e-8daf-75ee742dd458.csv", 
  # "EXP_03803a78-473e-49e6-8ac4-1893d4b225b2.csv",  
  # "EXP_baab7727-da3b-44f0-969b-a06d121c3b96.csv",
  # "EXP_1e86feb0-d169-4535-b642-6d709e5adbf7.csv",

    # "EXP_615a62db-aeca-4b7e-8daf-75ee742dd458.csv",
    # "EXP_03803a78-473e-49e6-8ac4-1893d4b225b2.csv",
    # "EXP_2b2d3119-a7ac-4175-9f60-485b4967e7f9.csv",
    # "EXP_1e86feb0-d169-4535-b642-6d709e5adbf7.csv" 


    # "EXP_2efee0d6-cd66-4866-8434-ff01497640d6.csv",
    # "EXP_108d191a-ac30-4943-9e2d-868426508408.csv",
    # "EXP_888296a3-2338-44f0-b13c-30cf35ddb21f.csv",
    # "EXP_ca49eedf-1fee-4e21-89ae-8ab8aa401b6d.csv",
    # "EXP_d4661b30-ad41-49e5-8e07-dd154ddd9424.csv",
    # "EXP_da34336c-cedd-4a23-b067-aa99fbf95cd4.csv",
    # "EXP_511d1281-c796-44fe-a271-95892ef12f9d.csv",
    # "EXP_b4e90536-0087-4531-8cca-b42926e2d1bb.csv",
    # "EXP_2a20cf08-a7c3-436d-bc31-90e8385f125b.csv", 
    # "EXP_93119221-c227-4423-b97a-91dfa6e83b74.csv", 
    # "EXP_e8d1e296-27cf-4212-a16a-4ade4a45def9.csv", 
    # "EXP_cfdfeee6-b1b7-43b9-88ed-32b774d64a30.csv",
    # "EXP_094db622-e7c5-4596-9c95-20d715b8db52.csv", 
    # "EXP_db8c804f-3c8f-4201-878c-c3a8d07245de.csv", 
    # "EXP_94412660-9659-48b2-ac24-9a8bd1a9eeb5.csv", 
    "EXP_929e1537-00cb-4576-b2c8-579c8c561e65.csv", 
    "EXP_17d848d9-f7e0-4cd5-94b9-830b9b83f2b5.csv", 
    "EXP_5b9a8497-c577-4c21-aca1-1a58b55d7f01.csv",
    "EXP_e1b4b5d7-1dd5-4c85-96e7-1d99dc1c6e54.csv"




    # # monthbyage Month Partions 
    # "EXP_a0b83a33-1134-48c9-86fc-0670e25d88e9.csv",
    # "EXP_1a024b8a-7b2b-4d19-baa6-c685d164ce35.csv", 
    # 'EXP_f60656d1-b0f3-41c2-a541-f37c77ba6435.csv', 
    # 'EXP_ea305efd-4f05-4b72-9788-f32c1cb329cc.csv',
    # "EXP_d13bd280-e2f5-41fb-891b-8fc92184afcb.csv",
    # "EXP_2b5b5bc6-042e-4663-96e6-b65e93b52611.csv", 
    # "EXP_fa8456e5-7a57-41cc-84fe-943abeb9b69d.csv", 
    # 'EXP_7cc6a6d0-d8d8-4b87-be8e-7e0b00628e24.csv'





    # # Calender Month Partitions
    # "EXP_9a268ddd-5055-409b-96f8-fcc67010dad8.csv",
    # "EXP_053fac26-c921-4a11-a7d0-da10a6d17a01.csv",
    # "EXP_94e1a18c-5451-457f-a6f8-4b984d3cba94.csv",
    # "EXP_ef87548a-b8de-4282-90c7-506705a66982.csv", 
    # "EXP_faec67e8-ee70-4fe1-a9b0-cd13a3565627.csv", 
    # "EXP_f747f6b2-08f2-41ec-8a72-6345889a676d.csv",
    # "EXP_ac4f3f71-b531-4a0a-a92e-223785bb173d.csv",
    # "EXP_010c050f-174a-4c51-8757-051a7202b9ec.csv", 
    # "EXP_5859c0d4-4100-407d-98c8-110371cd5435.csv", 
    # "EXP_742d9112-5b06-47ec-b85d-9b3172ea548c.csv", 
    # "EXP_d7c596b8-afc4-4d5a-ab9b-2eb8a9c2ad16.csv", 
    # "EXP_fe8549c1-5701-4352-9be3-55a74459dbb7.csv", 
    # "EXP_3db1240f-708d-451e-854f-20ce39a17580.csv", 
    # "EXP_76e13270-fc16-4c43-abc6-6bbeb3428383.csv", 
    # "EXP_205f7ff6-26ce-4575-bc98-77dc9ee0ed3d.csv",
    # "EXP_90c90f99-9e64-4a6f-8939-121b2a02e4b6.csv", 
    # "EXP_71739931-a713-4669-9b3d-0930feed236b.csv",
    # "EXP_c817e21e-0772-48c0-9d2f-c7c0afb7ef38.csv"

    # Simple Representation by Country 
    # "EXP_233b8cf2-57c4-4a5f-b020-76cff87e1cec.csv",
    # "EXP_7fcc2eca-f963-4309-a410-ef032b28db0e.csv", 
    # "EXP_cfabde97-e80a-49dd-b7ec-f9467fe66abc.csv", 
    # "EXP_494fb628-ef95-4d88-9794-706d8c0b8d0e.csv", 
    # "EXP_2ef15310-78b9-4893-a562-362b9c1a8b27.csv", 
    # "EXP_28ed0d0f-5d26-4386-8ee6-91a4ff59c835.csv", 
    # "EXP_15e739c4-65f2-4253-bdbc-c44a18874fdb.csv", 
    # "EXP_b160ef50-dd66-4940-9b2b-11dd5c8f11d0.csv", 
    # "EXP_62c49381-f042-4dc4-91be-ae3c4f3e2f18.csv", 
    # "EXP_b46240a3-a14e-4dac-b82c-4e84ba3ac975.csv", 
    # "EXP_84e8f1f3-12d0-497e-a90d-706bab032982.csv", 
    # "EXP_5152d4b2-fea1-4816-9cf4-fecedaab15cf.csv", 
    # "EXP_da812d61-4632-4593-b9b3-f4396d848c7f.csv", 
    # "EXP_2f5a2db8-eab3-41f9-8182-1010d3d4210a.csv"

    #Simple Representation by Cuntry: Specific Timepoints (25Mar21)
    # "EXP_b111b424-09f8-4cee-9d42-df7d7f7c0fab.csv",
    # "EXP_f3b12c1f-7c74-447f-98b8-b7c0d9e35d30.csv",
    # "EXP_c29fc61c-2b0d-42bf-84cf-ed6c4037de04.csv",
    # "EXP_30028685-ddc6-4769-899d-ec305dd9e71f.csv",
    # "EXP_5d5db3a2-e2e7-4626-b90b-81890f22c147.csv",
    # "EXP_a4674492-e184-4372-a565-377be20f9796.csv",
    # "EXP_efad42f2-673d-4aaf-87fb-ad318a28e469.csv",
    # "EXP_6b4ea009-2156-460a-bb04-1a513beeb63e.csv",
    # "EXP_9e45472d-53b4-49af-89c2-ce52a0355b15.csv",
    # "EXP_1a7dbcb0-14ea-404b-84a5-fa4bc8c8e0de.csv",
    # "EXP_8de70f19-5b89-4cc0-80c7-8ac1e8fed7a3.csv",
    # "EXP_825e28cc-2977-4c04-b7b2-dba448e5a790.csv",
    # "EXP_3e8ebc5c-c4c3-4718-bd4f-e31de1782ce8.csv",
    # "EXP_4d11afe5-83bb-4e0a-865d-1be7ad7948dd.csv",
    # "EXP_c9469e4b-8106-4fbd-ae6e-6d9a1ffd294c.csv",
    # "EXP_9f8eeba0-6fae-4dd6-9502-0afa0ff5db2f.csv",
    # "EXP_73b15355-dedc-4326-be60-109b04a9fb8e.csv"
]