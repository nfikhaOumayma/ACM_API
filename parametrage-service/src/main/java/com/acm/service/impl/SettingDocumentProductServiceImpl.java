/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Service;

import com.acm.aop.history.ProcessHistorySetting;
import com.acm.client.UserClient;
import com.acm.constants.common.CommonAOPConstants;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.SettingDocumentProductRepository;
import com.acm.service.SettingDocumentProductService;
import com.acm.service.SettingHistoriqueService;
import com.acm.utils.dtos.SettingDocumentProductDTO;
import com.acm.utils.dtos.SettingHistoriqueDTO;
import com.acm.utils.models.QSettingDocumentProduct;
import com.acm.utils.models.SettingDocumentProduct;
import com.acm.utils.models.SettingDocumentType;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link SettingDocumentProductServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 0.7.0
 */
@Service
public class SettingDocumentProductServiceImpl implements SettingDocumentProductService {

	/** The Constant logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(SettingDocumentProductServiceImpl.class);

	/** The settingDocumentProduct repository. */
	@Autowired
	private SettingDocumentProductRepository settingDocumentProductRepository;

	/** The setting historique service. */
	@Autowired
	private SettingHistoriqueService settingHistoriqueService;

	/** The user Client client. */
	@Autowired
	private UserClient userClient;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The Environment. */
	@Autowired
	private Environment environment;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingDocumentProductService#find(com.acm.utils.dtos.
	 * SettingDocumentProductDTO)
	 */
	@Override
	public List<SettingDocumentProductDTO> find(
			SettingDocumentProductDTO settingDocumentProductDTO) {

		// init QSettingDocumentProduct
		QSettingDocumentProduct qSettingDocumentProduct =
				QSettingDocumentProduct.settingDocumentProduct;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find by ID
		if (!ACMValidationUtils.isNullOrEmpty(settingDocumentProductDTO.getId())) {
			predicate.and(qSettingDocumentProduct.id.eq(settingDocumentProductDTO.getId()));
		}

		// find by id produit
		if (!ACMValidationUtils.isNullOrEmpty(settingDocumentProductDTO.getProductId())) {
			predicate.and(
					qSettingDocumentProduct.productId.eq(settingDocumentProductDTO.getProductId()));
		}

		// find by category
		if (!ACMValidationUtils.isNullOrEmpty(settingDocumentProductDTO.getSettingDocumentTypeDTO())
				&& !ACMValidationUtils.isNullOrEmpty(
						settingDocumentProductDTO.getSettingDocumentTypeDTO().getCategorie())) {
			predicate.and(qSettingDocumentProduct.settingDocumentType.categorie
					.eq(settingDocumentProductDTO.getSettingDocumentTypeDTO().getCategorie()));
		}

		// find by document type
		if (!ACMValidationUtils.isNullOrEmpty(settingDocumentProductDTO.getSettingDocumentTypeDTO())
				&& !ACMValidationUtils.isNullOrEmpty(
						settingDocumentProductDTO.getSettingDocumentTypeDTO().getId())) {
			predicate.and(qSettingDocumentProduct.settingDocumentType.eq(new SettingDocumentType(
					settingDocumentProductDTO.getSettingDocumentTypeDTO().getId())));
		}

		// find all
		if (Boolean.FALSE.equals(settingDocumentProductDTO.getAll())
				|| ACMValidationUtils.isNullOrEmpty(settingDocumentProductDTO.getAll())) {
			// find only enabled data
			predicate.and(qSettingDocumentProduct.enabled.eq(Boolean.TRUE));
		}

		// QueryDSL using springDATA
		Iterable<SettingDocumentProduct> iterable =
				settingDocumentProductRepository.findAll(predicate);
		List<SettingDocumentProduct> settingDocumentProducts = new ArrayList<>();
		iterable.forEach(settingDocumentProducts::add);
		logger.info("{} : setting Documents type Product was founded",
				settingDocumentProducts.size());

		// mapping returned list
		List<SettingDocumentProductDTO> settingDocumentProductDTOs = new ArrayList<>();
		settingDocumentProducts.forEach(settingDocumentProduct -> settingDocumentProductDTOs
				.add(mapper.map(settingDocumentProduct, SettingDocumentProductDTO.class)));

		logger.info("Returning founded data ...");
		return settingDocumentProductDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingDocumentProductService#save(com.acm.utils.dtos.
	 * SettingDocumentProductDTO)
	 */
	@Override
	@ProcessHistorySetting(action = CommonAOPConstants.NEW)
	public SettingDocumentProductDTO save(SettingDocumentProductDTO settingDocumentProductDTO) {

		Preconditions.checkNotNull(settingDocumentProductDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		SettingDocumentProduct settingDocumentProduct =
				mapper.map(settingDocumentProductDTO, SettingDocumentProduct.class);

		CommonFunctions.mapperToSave(settingDocumentProduct, userClient, logger);
		settingDocumentProduct.setEnabled(Boolean.FALSE);
		SettingDocumentProduct newSettingDocumentProduct =
				settingDocumentProductRepository.save(settingDocumentProduct);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE,
				SettingDocumentProduct.class.getSimpleName());
		return mapper.map(newSettingDocumentProduct, SettingDocumentProductDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingDocumentProductService#save(java.lang.Long,
	 * com.acm.utils.dtos.SettingDocumentProductDTO)
	 */
	@Override
	public SettingDocumentProductDTO save(Long id,
			SettingDocumentProductDTO settingDocumentProductDTO) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(settingDocumentProductDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Update SettingDocumentProduct with ID = {}", id);
		SettingDocumentProduct oldSettingDocumentProduct =
				settingDocumentProductRepository.findById(id).orElse(null);

		// check if object is null
		if (oldSettingDocumentProduct == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					SettingDocumentProduct.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + SettingDocumentProduct.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}

		// init settingHistory object
		SettingHistoriqueDTO settingHistoriqueDTO = new SettingHistoriqueDTO(
				CommonFunctions.getTableNameFromClass(SettingDocumentProduct.class),
				CommonAOPConstants.UPDATE, id, CommonFunctions.convertObjectToJSONString(
						mapper.map(oldSettingDocumentProduct, SettingDocumentProductDTO.class)));

		// mapping new data with existing data (oldSettingDocumentProduct)
		mapper.map(settingDocumentProductDTO, oldSettingDocumentProduct);
		CommonFunctions.mapperToUpdate(oldSettingDocumentProduct, userClient, logger);

		// update & persist data in DB
		SettingDocumentProduct newSettingDocumentProduct =
				settingDocumentProductRepository.save(oldSettingDocumentProduct);
		SettingDocumentProductDTO newSettingDocumentProductDTO =
				mapper.map(newSettingDocumentProduct, SettingDocumentProductDTO.class);

		// saving history setting
		settingHistoriqueDTO.setUpdatedBy(newSettingDocumentProduct.getUpdatedBy());
		settingHistoriqueDTO.setNewData(
				CommonFunctions.convertObjectToJSONString(newSettingDocumentProductDTO));
		settingHistoriqueService.save(settingHistoriqueDTO);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE,
				SettingDocumentProduct.class.getSimpleName());
		return newSettingDocumentProductDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.SettingDocumentProductService#disableDocumentTypeProduct(com.acm.utils.dtos.
	 * SettingDocumentProductDTO)
	 */
	@Override
	public void updateStatus(SettingDocumentProductDTO settingDocumentProductDTO, Boolean status)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(settingDocumentProductDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(settingDocumentProductDTO.getId(),
				CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.warn("update status settingDocumentProductDTO  with ID = {}",
				settingDocumentProductDTO.getId());
		// update status object by id
		SettingDocumentProductDTO newSettingDocumentProductDTO =
				find(settingDocumentProductDTO.getId());
		newSettingDocumentProductDTO.setEnabled(status);
		save(newSettingDocumentProductDTO.getId(), newSettingDocumentProductDTO);
		logger.info("update status Document Product  with ID = {} :: DONE",
				settingDocumentProductDTO.getId());

	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingDocumentProductService#find(java.lang.Long)
	 */
	@Override
	public SettingDocumentProductDTO find(Long id) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find settingDocumentProduct by ID : {}", id);
		SettingDocumentProduct settingDocumentProduct =
				settingDocumentProductRepository.findById(id).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(settingDocumentProduct)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					SettingDocumentProduct.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("exception.message.not.found")
							+ SettingDocumentProduct.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		return mapper.map(settingDocumentProduct, SettingDocumentProductDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingDocumentProductService#find()
	 */
	@Override
	public List<SettingDocumentProductDTO> find() {

		List<SettingDocumentProduct> settingDocumentProducts =
				settingDocumentProductRepository.findAll();
		List<SettingDocumentProductDTO> settingDocumentProductDTOs = new ArrayList<>();
		settingDocumentProducts.forEach(settingDocumentProduct -> settingDocumentProductDTOs
				.add(mapper.map(settingDocumentProduct, SettingDocumentProductDTO.class)));
		return settingDocumentProductDTOs;
	}
}
