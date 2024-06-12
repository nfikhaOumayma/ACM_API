/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.acm.client.UserClient;
import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.AcmCollateralRepository;
import com.acm.service.AcmCollateralService;
import com.acm.service.UserDefinedFieldsLinksService;
import com.acm.utils.dtos.AcmCollateralDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.UserDefinedFieldsLinksDTO;
import com.acm.utils.models.AcmCollateral;
import com.acm.utils.models.Loan;
import com.acm.utils.models.QAcmCollateral;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link AcmCollateralServiceImpl} class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
@Service
public class AcmCollateralServiceImpl implements AcmCollateralService {
	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(AcmCollateralServiceImpl.class);

	/** The acm collateral repository. */
	@Autowired
	private AcmCollateralRepository acmCollateralRepository;
	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The user Client client. */
	@Autowired
	private UserClient userClient;

	/** The user defined fields links service. */
	@Autowired
	private UserDefinedFieldsLinksService userDefinedFieldsLinksService;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmCollateralService#find(com.acm.utils.dtos.AcmCollateralDTO)
	 */
	@Override
	public List<AcmCollateralDTO> find(AcmCollateralDTO acmCollateralDTO) {

		// init
		QAcmCollateral qAcmCollateral = QAcmCollateral.acmCollateral;

		// returning empty list if ID loan is null
		if ((ACMValidationUtils.isNullOrEmpty(acmCollateralDTO.getLoan())
				&& ACMValidationUtils.isNullOrEmpty(acmCollateralDTO.getLoan().getLoanId())
				&& ACMValidationUtils.isNullOrEmpty(acmCollateralDTO.getExternLoanId())
				&& ACMValidationUtils.isNullOrEmpty(acmCollateralDTO.getCustomer().getId()))) {
			return new ArrayList<>();
		}

		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();
		// find only enabled data
		predicate.and(qAcmCollateral.enabled.eq(Boolean.TRUE));
		// find by Id acm_loan
		if (!ACMValidationUtils.isNullOrEmpty(acmCollateralDTO.getLoan())
				&& !ACMValidationUtils.isNullOrEmpty(acmCollateralDTO.getLoan().getLoanId())) {
			predicate.and(qAcmCollateral.loan.idLoan.eq(acmCollateralDTO.getLoan().getLoanId()));
		}
		// find by Id loan extern
		if (!ACMValidationUtils.isNullOrEmpty(acmCollateralDTO.getLoan()) && !ACMValidationUtils
				.isNullOrEmpty(acmCollateralDTO.getLoan().getIdLoanExtern())) {
			predicate.and(qAcmCollateral.loan.idLoanExtern
					.eq(acmCollateralDTO.getLoan().getIdLoanExtern()));
		}
		// find by Id loan extern
		if (!ACMValidationUtils.isNullOrEmpty(acmCollateralDTO.getExternLoanId())) {
			predicate.and(qAcmCollateral.externLoanId.eq(acmCollateralDTO.getExternLoanId()));
		}

		Iterable<AcmCollateral> iterable = acmCollateralRepository.findAll(predicate);
		List<AcmCollateral> acmCollaterals = new ArrayList<>();
		iterable.forEach(acmCollaterals::add);
		logger.info("{} : acmCollaterals was founded", acmCollaterals.size());

		// mapping data
		List<AcmCollateralDTO> acmCollateralDTOs = new ArrayList<>();
		acmCollaterals.forEach(acmCollateral -> acmCollateralDTOs
				.add(mapper.map(acmCollateral, AcmCollateralDTO.class)));
		return acmCollateralDTOs;

	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmCollateralService#save(com.acm.utils.dtos.AcmCollateralDTO)
	 */
	@Override
	public AcmCollateralDTO save(AcmCollateralDTO acmCollateralDTO) {

		Preconditions.checkNotNull(acmCollateralDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		AcmCollateral acmCollateral = mapper.map(acmCollateralDTO, AcmCollateral.class);
		acmCollateral.setIsDeleted(Boolean.FALSE);
		acmCollateral.setWithHoldingRate(BigDecimal.ZERO);
		CommonFunctions.mapperToSave(acmCollateral, userClient, logger);
		AcmCollateral result = acmCollateralRepository.save(acmCollateral);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, AcmCollateral.class.getSimpleName());
		return mapper.map(result, AcmCollateralDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmCollateralService#save(java.lang.Long,
	 * com.acm.utils.dtos.AcmCollateralDTO)
	 */
	@Override
	public AcmCollateralDTO save(Long id, AcmCollateralDTO acmCollateralDTO) {

		Preconditions.checkNotNull(acmCollateralDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		AcmCollateral oldCollateral = acmCollateralRepository.findById(id).orElse(null);

		oldCollateral.setIdAcmCollateral(id);
		oldCollateral.setReference(acmCollateralDTO.getReference());
		oldCollateral.setDescription(acmCollateralDTO.getDescription());
		oldCollateral.setCollateralTypeIdExtern(acmCollateralDTO.getCollateralTypeIdExtern());
		oldCollateral.setCollateralTypeDescription(acmCollateralDTO.getCollateralTypeDescription());
		oldCollateral.setOriginalGrossValue(acmCollateralDTO.getOriginalGrossValue());
		oldCollateral.setGrossValue(acmCollateralDTO.getGrossValue());
		oldCollateral.setRealisedValue(acmCollateralDTO.getRealisedValue());
		oldCollateral.setFixedCost(acmCollateralDTO.getFixedCost());
		oldCollateral.setNetValue(acmCollateralDTO.getNetValue());
		oldCollateral.setValueDate(acmCollateralDTO.getValueDate());
		oldCollateral.setExpiryDate(acmCollateralDTO.getExpiryDate());
		oldCollateral.setValuerId(acmCollateralDTO.getValuerId());
		oldCollateral.setValuerName(acmCollateralDTO.getValuerName());
		oldCollateral.setEnabled(acmCollateralDTO.getEnabled());
		oldCollateral.setIsDeleted(Boolean.FALSE);
		oldCollateral.setWithHoldingRate(BigDecimal.ZERO);
		CommonFunctions.mapperToUpdate(oldCollateral, userClient, logger);
		AcmCollateral result = acmCollateralRepository.save(oldCollateral);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, AcmCollateral.class.getSimpleName());
		return mapper.map(result, AcmCollateralDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmCollateralService#save(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public void save(LoanDTO loanDTO) {

		Preconditions.checkNotNull(loanDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(loanDTO.getLoanId(),
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Begin addGuarantie method method...");
		// delete collateral by id loan
		acmCollateralRepository.deleteByLoan(new Loan(loanDTO.getLoanId()));
		// Add All relation
		for (AcmCollateralDTO acmCollateralDTO : loanDTO.getCollaterals()) {
			// saving data in ACM
			save(acmCollateralDTO);
		}
		logger.info("Saving All  Guaranties for given Loan : {} :: DONE",
				loanDTO.getAccountNumber());
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmCollateralService#delete(com.acm.utils.dtos.AcmCollateralDTO)
	 */
	@Override
	public void delete(AcmCollateralDTO acmCollateralDTO) {

		Preconditions.checkNotNull(acmCollateralDTO, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		acmCollateralRepository.delete(new AcmCollateral(acmCollateralDTO.getIdAcmCollateral()));
		logger.info("Delete AcmCollateral with ID = {}", acmCollateralDTO.getIdAcmCollateral());

	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmCollateralService#saveOrUpdateOrDelete(com.acm.utils.dtos.
	 * AcmCollateralDTO)
	 */
	@Override
	public AcmCollateralDTO saveOrUpdateOrDelete(AcmCollateralDTO acmCollateralDTO)
			throws ResourcesNotFoundException {

		AcmCollateralDTO result = null;
		switch (acmCollateralDTO.getAction()) {
			case CommonConstants.ACTION_INSERT:
				result = save(acmCollateralDTO);
				break;
			case CommonConstants.ACTION_UPDATE:
				result = save(acmCollateralDTO.getIdAcmCollateral(), acmCollateralDTO);
				break;
			case CommonConstants.ACTION_DELETE:
				if (ACMValidationUtils
						.isNullOrEmpty(acmCollateralDTO.getUserDefinedFieldsLinksDTOs())) {
					UserDefinedFieldsLinksDTO udfLinkParam = new UserDefinedFieldsLinksDTO();
					udfLinkParam.setElementId(acmCollateralDTO.getIdAcmCollateral());
					udfLinkParam.setCategory(CommonConstants.COLLATERAL_CATEGORY);
					acmCollateralDTO.setUserDefinedFieldsLinksDTOs(
							userDefinedFieldsLinksService.find(udfLinkParam));
				}
				acmCollateralDTO.getUserDefinedFieldsLinksDTOs()
						.forEach(udfLink -> udfLink.setFieldValue(null));
				delete(acmCollateralDTO);
				break;
		}

		userDefinedFieldsLinksService.updateAcmUdfLinksByElementId(
				acmCollateralDTO.getUserDefinedFieldsLinksDTOs(),
				(result != null && result.getIdAcmCollateral() != null)
						? result.getIdAcmCollateral()
						: acmCollateralDTO.getIdAcmCollateral(),
				CommonConstants.COLLATERAL_CATEGORY);
		return result;
	}
}
