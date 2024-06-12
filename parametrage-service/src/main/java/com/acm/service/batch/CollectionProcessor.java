/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.batch;

import java.util.Date;
import java.util.List;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.item.ItemProcessor;
import org.springframework.beans.factory.annotation.Autowired;

import com.acm.client.CreditClient;
import com.acm.constants.common.CommonConstants;
import com.acm.service.AcmCollectionService;
import com.acm.utils.dtos.AcmCollectionDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.enums.CollectionStatus;
import com.acm.utils.models.AcmCollection;
import com.acm.utils.validation.ACMValidationUtils;

/**
 * The Class CollectionProcessor.
 */
public class CollectionProcessor implements ItemProcessor<AcmCollectionDTO, AcmCollection> {
	private static final Logger logger = LoggerFactory.getLogger(CollectionProcessor.class);

	/** The acm collection service. */
	@Autowired
	private AcmCollectionService acmCollectionService;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	@Autowired
	private CreditClient creditClient;

	/*
	 * (non-Javadoc)
	 * @see org.springframework.batch.item.ItemProcessor#process(java.lang.Object)
	 */
	@Override
	public AcmCollection process(AcmCollectionDTO data) throws Exception {

		AcmCollection collection = null;

		if (!ACMValidationUtils.isNullOrEmpty(data)
				&& !ACMValidationUtils.isNullOrEmpty(data.getAccountNumber())) {
			
			logger.info("Process collection item with account number {} ", data.getAccountNumber());

			// init collection model
			collection = new AcmCollection();

			// find by account number
			AcmCollectionDTO params = new AcmCollectionDTO();
			params.setAccountNumber(data.getAccountNumber());
			// Get only Collections records
			params.setCollectionType(CommonConstants.COLLECTION_CATEGORY);
			List<AcmCollectionDTO> acmCollectionDTOs =
					acmCollectionService.find(params, Boolean.TRUE);

			// Filter for collection not closed (for all other status need to update)
			AcmCollectionDTO existingColl = null;
			if (!ACMValidationUtils.isNullOrEmpty(acmCollectionDTOs)) {
				existingColl = acmCollectionDTOs.stream().filter(
						coll -> !coll.getStatus().equals(CollectionStatus.CLOSED.statusId()))
						.findAny().orElse(null);
			}
			// if not exist in status Active or Completed then create new collection record
			if (ACMValidationUtils.isNullOrEmpty(existingColl)) {

				// find loan in acm
				try {
					LoanDTO loanDTO = creditClient.findByIdExtern(data.getIdLoanExtern());
					if (ACMValidationUtils.isNullOrEmpty(loanDTO)) {
						logger.info("Loan doesn't exists on ACM, account number {}",
								data.getAccountNumber());
						return null;
					}
				}
				catch (Exception e) {
					logger.error("Error while getting ACM loan for collections {} = {}",
							data.getAccountNumber(), e.getMessage());
					return null;
				}

				// mapping data as it's a new collection
				collection = mapper.map(data, AcmCollection.class);
				// set insetion date
				collection.setDateInsertion(new Date());
				collection.setCollectionType(CommonConstants.COLLECTION_CATEGORY);
				// enable true
				collection.setEnabled(Boolean.TRUE);
				collection.setStatus(CollectionStatus.ACTIVE.statusId());
				// set user by default
				collection.setInsertBy("Collection Batch");
				logger.info("init new collection item with account number {} ",
						collection.getAccountNumber());

			}
			// else the collection already exist it need only update
			else {
				// mapping abacus collection with the existing acm collection in acm
				// set old data of acm collection in the updated collection from abacus

				data.setId(existingColl.getId());
				data.setEnabled(existingColl.getEnabled());
				data.setDateInsertion(existingColl.getDateInsertion());
				data.setInsertBy(existingColl.getInsertBy());
				data.setStatus(existingColl.getStatus());
				data.setCollectionType(existingColl.getCollectionType());
				data.setIdAcmCollectionStep(existingColl.getIdAcmCollectionStep());
				data.setStatutLibelle(existingColl.getStatutLibelle());
				data.setStatutLibelleDone(existingColl.getStatutLibelleDone());
				data.setAvailableDate(existingColl.getAvailableDate());
				data.setOwner(existingColl.getOwner());
				data.setOwnerName(existingColl.getOwnerName());
				data.setGroupOwner(existingColl.getGroupOwner());
				data.setGroupOwnerName(existingColl.getGroupOwnerName());
				data.setStatutWorkflow(existingColl.getStatutWorkflow());


				collection = mapper.map(data, AcmCollection.class);
				// set date last update
				collection.setDateLastUpdate(new Date());
				// set user by default
				collection.setUpdatedBy("Collection Batch");
				logger.info("Set collection item for update, account number {} ",
						collection.getAccountNumber());

			}

		}
		return collection;

	}

}
