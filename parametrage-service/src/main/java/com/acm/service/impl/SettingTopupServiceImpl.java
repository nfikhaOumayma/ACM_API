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
import org.springframework.stereotype.Service;

import com.acm.client.CreditClient;
import com.acm.client.TransversClient;
import com.acm.client.UserClient;
import com.acm.constants.common.CommonErrorCode;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.model.ExceptionResponseMessage;
import com.acm.exceptions.type.ParametrageException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.SettingTopupRepository;
import com.acm.service.ProductService;
import com.acm.service.SettingTopupService;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.ProductDTO;
import com.acm.utils.dtos.SettingTopupDTO;
import com.acm.utils.dtos.SettingTopupValidityDTO;
import com.acm.utils.models.ProductDetails;
import com.acm.utils.models.QSettingTopup;
import com.acm.utils.models.SettingTopup;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link SettingTopupServiceImpl} class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
@Service
public class SettingTopupServiceImpl implements SettingTopupService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(SettingTopupServiceImpl.class);

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The user client. */
	@Autowired
	private UserClient userClient;

	/** The setting topup repository. */
	@Autowired
	private SettingTopupRepository settingTopupRepository;

	/** The product service. */
	@Autowired
	private ProductService productService;

	/** The transvers client. */
	@Autowired
	private TransversClient transversClient;

	/** The credit client. */
	@Autowired
	private CreditClient creditClient;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingTopupService#find(com.acm.utils.dtos.SettingTopupDTO)
	 */
	@Override
	public List<SettingTopupDTO> find(SettingTopupDTO settingTopupDTO) {

		// init qSettingTopup
		QSettingTopup qSettingTopup = QSettingTopup.settingTopup;

		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();
		// get only enabled data
		predicate.and(qSettingTopup.enabled.eq(Boolean.TRUE));

		if (!ACMValidationUtils.isNullOrEmpty(settingTopupDTO.getProductId())) {
			// find by product id
			predicate.and(qSettingTopup.productId.eq(settingTopupDTO.getProductId()));
		}

		// QueryDSL using springDATA
		Iterable<SettingTopup> iterable = settingTopupRepository.findAll(predicate);
		List<SettingTopup> settingTopups = new ArrayList<>();
		iterable.forEach(settingTopups::add);

		// mapping returned list
		List<SettingTopupDTO> settingTopupDTOs = new ArrayList<>();
		settingTopups.forEach(settingTopup -> settingTopupDTOs
				.add(mapper.map(settingTopup, SettingTopupDTO.class)));

		logger.info("Returning founded data");
		return settingTopupDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingTopupService#save(com.acm.utils.dtos.SettingTopupDTO)
	 */
	@Override
	public SettingTopupDTO save(SettingTopupDTO settingTopupDTO) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(settingTopupDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		SettingTopup settingTopup = mapper.map(settingTopupDTO, SettingTopup.class);
		CommonFunctions.mapperToSave(settingTopup, userClient, logger);
		SettingTopup newSettingTopup = settingTopupRepository.save(settingTopup);
		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, ProductDetails.class.getSimpleName());
		SettingTopupDTO settingTopupDTOResult = mapper.map(newSettingTopup, SettingTopupDTO.class);
		// update product with new topup setting
		ProductDTO productDTO = productService.find(settingTopupDTO.getProductId());
		productDTO.setSettingTopup(settingTopupDTOResult);
		productService.save(productDTO.getId(), productDTO);
		return mapper.map(newSettingTopup, SettingTopupDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingTopupService#save(java.lang.Long,
	 * com.acm.utils.dtos.SettingTopupDTO)
	 */
	@Override
	public SettingTopupDTO save(Long id, SettingTopupDTO settingTopupDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(settingTopupDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Update SettingTopup with ID = {}", id);
		SettingTopup oldSettingTopup = settingTopupRepository.findById(id).orElse(null);

		// check if object is null
		if (oldSettingTopup == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					ProductDetails.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + SettingTopup.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		// mapping new data with existing data (oldProductDetails)
		mapper.map(settingTopupDTO, oldSettingTopup);
		CommonFunctions.mapperToUpdate(oldSettingTopup, userClient, logger);

		// update & persist data in DB
		SettingTopup newSettingTopup = settingTopupRepository.save(oldSettingTopup);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, SettingTopup.class.getSimpleName());
		return mapper.map(newSettingTopup, SettingTopupDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingTopupService#checkValidity(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public SettingTopupValidityDTO checkValidity(LoanDTO loanDTO) throws ParametrageException {

		// init settingTopupValidityDTO
		SettingTopupValidityDTO settingTopupValidityDTO = new SettingTopupValidityDTO();
		// get product
		ProductDTO productDTO =
				productService.find(new ProductDTO(loanDTO.getProductDTO().getProductIdAbacus()))
						.stream().findFirst().orElse(null);
		// if product not found then throw exception
		if (ACMValidationUtils.isNullOrEmpty(productDTO)) {
			throw new ParametrageException(
					new ExceptionResponseMessage(CommonErrorCode.PRODUCT_NOT_FOUND,
							CommonExceptionsMessage.ERROR_PRODUCT_NOT_FOUND),
					CommonExceptionsMessage.ERROR_PRODUCT_NOT_FOUND);
		}
		loanDTO.setProductDTO(productDTO);
		// get setting topup of the product
		SettingTopupDTO settingTopupDTO = productDTO.getSettingTopup();
		// if there is topup setting
		if (!ACMValidationUtils.isNullOrEmpty(settingTopupDTO)) {
			// get all enabled setting topups
			List<SettingTopupDTO> settingTopupDTOs = find(new SettingTopupDTO());
			// init loanDTO with settingTopup list of all products
			loanDTO.setSettingTopupDTOs(settingTopupDTOs);
			// get check setting topup from abacus data
			settingTopupValidityDTO = transversClient.checkSettingTopupValidity(loanDTO);
			// get check setting topup from acm : check on number of topups on the same accountId
			Integer count = 0;
			count = creditClient.countTopupsByAccount(loanDTO.getIdAccountExtern());
			if (!ACMValidationUtils.isNullOrEmpty(settingTopupDTO.getTopupMaxAllowedTopups())
					&& settingTopupDTO.getTopupMaxAllowedTopups() <= count) {
				settingTopupValidityDTO.setMaxAllowedTopupsValidity(Boolean.FALSE);
			}
			else {
				settingTopupValidityDTO.setMaxAllowedTopupsValidity(Boolean.TRUE);
			}
		}
		return settingTopupValidityDTO;
	}
}
