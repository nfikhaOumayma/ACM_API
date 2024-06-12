/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.batch;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.item.ItemProcessor;
import org.springframework.beans.factory.annotation.Autowired;

import com.acm.client.ParametrageClient;
import com.acm.constants.common.CommonConstants;
import com.acm.service.AcmCollectionService;
import com.acm.utils.dtos.AcmCollectionDTO;
import com.acm.utils.dtos.CollectionStepDTO;
import com.acm.utils.enums.CollectionStatus;
import com.acm.utils.models.AcmCollection;
import com.acm.utils.validation.ACMValidationUtils;

/**
 * The Class CollectionProcessor.
 */
public class LegalCollectionProcessor implements ItemProcessor<AcmCollectionDTO, AcmCollection> {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(LegalCollectionProcessor.class);

	/** The acm collection service. */
	@Autowired
	private AcmCollectionService acmCollectionService;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The parametrage client. */
	@Autowired
	private ParametrageClient parametrageClient;

	/** The collections step settings. */
	private List<CollectionStepDTO> collectionsStepSettings = null;

	/*
	 * (non-Javadoc)
	 * @see org.springframework.batch.item.ItemProcessor#process(java.lang.Object)
	 */
	@Override
	public AcmCollection process(AcmCollectionDTO data) throws Exception {

		if (checkCollectionLegalWFSetting(data)) {

			logger.info("Process Legal collection item with account number {} ",
					data.getAccountNumber());

			// init collection model
			AcmCollection collection = new AcmCollection();

			// find by parent collection id
			AcmCollectionDTO params = new AcmCollectionDTO();
			params.setIdParentCollection(data.getId());
			params.setCollectionType(CommonConstants.LEGAL_CATEGORY);
			List<AcmCollectionDTO> acmCollectionDTOs =
					acmCollectionService.find(params, Boolean.TRUE);

			AcmCollectionDTO existingColl = null;
			if (!ACMValidationUtils.isNullOrEmpty(acmCollectionDTOs)) {
				existingColl = acmCollectionDTOs.get(0);
			}
			// if not exist in status Active or Completed then create new collection record
			if (ACMValidationUtils.isNullOrEmpty(existingColl)) {
				// mapping data as it's a new collection
				collection = mapper.map(data, AcmCollection.class);
				// need to be set null to create new line
				collection.setId(null);
				// set insetion date
				collection.setDateInsertion(new Date());
				collection.setCollectionType(CommonConstants.LEGAL_CATEGORY);
				collection.setIdParentCollection(data.getId());
				// enable true
				collection.setEnabled(Boolean.TRUE);
				collection.setStatus(CollectionStatus.ACTIVE.statusId());
				// set user by default
				collection.setInsertBy("Legal Collection Batch");
				logger.info("init new Legal collection item with account number {} ",
						collection.getAccountNumber());

			}
			// else the collection already exist it need only update
			else {
				// mapping with the returned data from ACM
				collection = mapper.map(data, AcmCollection.class);

				// set old data of legal collection in the updated collection
				collection.setId(existingColl.getId());
				collection.setEnabled(existingColl.getEnabled());
				collection.setDateInsertion(existingColl.getDateInsertion());
				collection.setInsertBy(existingColl.getInsertBy());
				collection.setStatus(existingColl.getStatus());
				collection.setCollectionType(existingColl.getCollectionType());
				collection.setIdAcmCollectionStep(existingColl.getIdAcmCollectionStep());
				collection.setIdParentCollection(existingColl.getIdParentCollection());
				collection.setAvailableDate(existingColl.getAvailableDate());
				collection.setOwner(existingColl.getOwner());
				collection.setOwnerName(existingColl.getOwnerName());
				collection.setGroupOwner(existingColl.getGroupOwner());
				collection.setGroupOwnerName(existingColl.getGroupOwnerName());

				collection.setStatutLibelle(existingColl.getStatutLibelle());
				collection.setStatutLibelleDone(existingColl.getStatutLibelleDone());

				// set date last update
				collection.setDateLastUpdate(new Date());
				// set user by default
				collection.setUpdatedBy("Legal Collection Batch");
				logger.info("Set Legal collection item for update, account number {} ",
						collection.getAccountNumber());

			}

			return collection;
		}
		else {
			return null;
		}

	}

	/**
	 * Check collection against setting.
	 *
	 * @param data the data
	 * @return true, if successful
	 */
	private boolean checkCollectionLegalWFSetting(AcmCollectionDTO data) {

		loadParametrageLegalCollection();
		if (!ACMValidationUtils.isNullOrEmpty(collectionsStepSettings)) {
			CollectionStepDTO settingProduct = collectionsStepSettings.stream()
					.filter(cs -> cs.getProductId().equals(data.getProductId())).findAny()
					.orElse(null);

			return !ACMValidationUtils.isNullOrEmpty(settingProduct)
					&& settingProduct.getLateDate().compareTo(data.getLateDays()) < 0
					&& settingProduct.getAmount().compareTo(data.getAmount()) < 0
					&& settingProduct.getUnpaidAmount().compareTo(data.getUnpaidAmount()) < 0;

		}
		return false;
	}

	/**
	 * Load parametrage legal collection.
	 */
	private void loadParametrageLegalCollection() {

		if (ACMValidationUtils.isNullOrEmpty(collectionsStepSettings)) {
			collectionsStepSettings = new ArrayList<CollectionStepDTO>();
			CollectionStepDTO collectionStepDTO = new CollectionStepDTO();
			// get the fist step setting of each product
			collectionStepDTO.setOrder(0L);
			collectionStepDTO.setEnabled(Boolean.TRUE);
			collectionStepDTO.setProcess(CommonConstants.LEGAL_CATEGORY);
			collectionsStepSettings = parametrageClient.findSettingCollection(collectionStepDTO);
			logger.info("Get Legal Collection Settings !");
		}
	}

}
