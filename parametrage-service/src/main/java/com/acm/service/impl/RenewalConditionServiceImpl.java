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
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.stereotype.Service;

import com.acm.client.TransversClient;
import com.acm.client.UserClient;
import com.acm.constants.common.CommonErrorCode;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.model.ExceptionResponseMessage;
import com.acm.exceptions.type.RenewalConditionSettingLoanException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.RenewalConditionRepository;
import com.acm.service.RenewalConditionService;
import com.acm.utils.dtos.CustomerAccountDTO;
import com.acm.utils.dtos.RenewalConditionDTO;
import com.acm.utils.dtos.RenewalConditionLoanDTO;
import com.acm.utils.models.QRenewalCondition;
import com.acm.utils.models.RenewalCondition;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * The Class RenewalConditionServiceImpl.
 */
@Service
public class RenewalConditionServiceImpl implements RenewalConditionService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(RenewalConditionServiceImpl.class);

	/** The renewal condition repository. */
	@Autowired
	private RenewalConditionRepository renewalConditionRepository;
	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The environment. */
	@Autowired
	private Environment environment;
	/** The user client. */
	@Autowired
	private UserClient userClient;

	/** The transvers client. */
	@Autowired
	private TransversClient transversClient;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.RenewalConditionService#find(java.lang.Long)
	 */
	@Override
	public RenewalConditionDTO find(Long idRenewalCondition) throws ResourcesNotFoundException {

		// check id not not null
		Preconditions.checkNotNull(idRenewalCondition, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find renewalCondition by ID : {}", idRenewalCondition);
		RenewalCondition renewalCondition =
				renewalConditionRepository.findById(idRenewalCondition).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(renewalCondition)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					RenewalCondition.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("exception.message.not.found")
							+ RenewalCondition.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + idRenewalCondition);
		}
		return mapper.map(renewalCondition, RenewalConditionDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.RenewalConditionService#find(com.acm.utils.dtos.RenewalConditionDTO)
	 */
	@Override
	public List<RenewalConditionDTO> find(RenewalConditionDTO renewalConditionDTO) {

		Preconditions.checkNotNull(renewalConditionDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);

		// init QRenewalCondition
		QRenewalCondition qRenewalCondition = QRenewalCondition.renewalCondition;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();
		// find only enabled data
		predicate.and(qRenewalCondition.enabled.eq(Boolean.TRUE));

		Iterable<RenewalCondition> iterable = renewalConditionRepository.findAll(predicate,
				Sort.by(Direction.ASC, "year", "minAmount"));
		List<RenewalCondition> renewalConditions = new ArrayList<>();
		iterable.forEach(renewalConditions::add);
		// mapping data
		List<RenewalConditionDTO> renewalConditionDTOs = new ArrayList<>();
		renewalConditions.forEach(renewalCondition -> renewalConditionDTOs
				.add(mapper.map(renewalCondition, RenewalConditionDTO.class)));
		logger.info("{} : renewalCondition was founded", renewalConditions.size());
		return renewalConditionDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.RenewalConditionService#save(com.acm.utils.dtos.RenewalConditionDTO)
	 */
	@Override
	public RenewalConditionDTO save(RenewalConditionDTO renewalConditionDTO) {

		Preconditions.checkNotNull(renewalConditionDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// CHECK DATA
		if (ACMValidationUtils.isNullOrEmpty(renewalConditionDTO.getYear())
				|| ACMValidationUtils.isNullOrEmpty(renewalConditionDTO.getMaxAmount())
				|| ACMValidationUtils.isNullOrEmpty(renewalConditionDTO.getMinAmount())
				|| ACMValidationUtils.isNullOrEmpty(renewalConditionDTO.getPourcentage())
				|| ACMValidationUtils.isNullOrEmpty(renewalConditionDTO.getOrdre())) {
			logger.warn("Failed to INSERT new row : check sended DATA");
			return null;
		}
		RenewalCondition renewalCondition = mapper.map(renewalConditionDTO, RenewalCondition.class);
		CommonFunctions.mapperToSave(renewalCondition, userClient, logger);

		RenewalCondition newRenewalCondition = renewalConditionRepository.save(renewalCondition);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, RenewalCondition.class.getSimpleName());
		return mapper.map(newRenewalCondition, RenewalConditionDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.RenewalConditionService#save(java.lang.Long,
	 * com.acm.utils.dtos.RenewalConditionDTO)
	 */
	@Override
	public RenewalConditionDTO save(Long id, RenewalConditionDTO renewalConditionDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(renewalConditionDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// CHECK DATA
		if (ACMValidationUtils.isNullOrEmpty(renewalConditionDTO.getYear())
				|| ACMValidationUtils.isNullOrEmpty(renewalConditionDTO.getMaxAmount())
				|| ACMValidationUtils.isNullOrEmpty(renewalConditionDTO.getMinAmount())
				|| ACMValidationUtils.isNullOrEmpty(renewalConditionDTO.getPourcentage())
				|| ACMValidationUtils.isNullOrEmpty(renewalConditionDTO.getOrdre())) {
			logger.warn("Failed to UPDATE row : check sended DATA");
			return null;
		}
		logger.info("Update RenewalCondition  with ID = {}", id);
		RenewalCondition oldRenewalCondition = renewalConditionRepository.findById(id).orElse(null);

		// check if object is null
		if (oldRenewalCondition == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					RenewalCondition.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + RenewalCondition.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		// mapping new data with existing data (oldRenewalCondition)
		mapper.map(renewalConditionDTO, oldRenewalCondition);
		CommonFunctions.mapperToUpdate(oldRenewalCondition, userClient, logger);
		RenewalCondition newRenewalCondition = renewalConditionRepository.save(oldRenewalCondition);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, RenewalCondition.class.getSimpleName());
		return mapper.map(newRenewalCondition, RenewalConditionDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.RenewalConditionService#delete(java.lang.Long)
	 */
	@Override
	public void delete(Long id) {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Delete Renewal Condition Settinge with ID = {}", id);
		// delete old Renewal Condition Settinge
		renewalConditionRepository.deleteById(id);
		logger.info(CommonLoggerMessage.SUCCESFULL_DELETE, RenewalCondition.class.getSimpleName());
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.RenewalConditionService#findByYearAndLastPaidAmount(java.lang.Integer,
	 * java.lang.Long)
	 */
	@Override
	public RenewalConditionDTO findByYearAndLastPaidAmount(Integer renewalYear, Long lastPaidAmount)
			throws ResourcesNotFoundException {

		RenewalCondition renewalCondition = renewalConditionRepository
				.findByYearAndLastPaidAmount(renewalYear, lastPaidAmount).get(0);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(renewalCondition)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					RenewalCondition.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("exception.message.not.found")
							+ RenewalCondition.class.getSimpleName());
		}
		return mapper.map(renewalCondition, RenewalConditionDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.RenewalConditionService#findRenewalConditionSetting(java.lang.Long)
	 */
	@Override
	public RenewalConditionLoanDTO findRenewalConditionSetting(
			CustomerAccountDTO customerAccountDTO) throws RenewalConditionSettingLoanException {

		List<Double> customerPaidAccounts = new ArrayList<>();
		RenewalConditionLoanDTO renewalConditionLoanDTO = new RenewalConditionLoanDTO();
		try {
			customerPaidAccounts =
					transversClient.findCustomerPaidAccounts(customerAccountDTO.getCustomerId());

		}
		catch (Exception e) {
			logger.error("Error will calling API ABACUS : {}", e.getMessage());
			return null;
		}
		// get renewal condition setting based on last paid amounts
		// renewal case of new loan or topup issued loan
		if (!ACMValidationUtils.isNullOrEmpty(customerPaidAccounts)
				|| Boolean.TRUE.equals(customerAccountDTO.getCanTopup())) {
			List<RenewalCondition> renewalConditions = new ArrayList<>();
			if (Boolean.TRUE.equals(customerAccountDTO.getCanTopup())) {

				renewalConditions = renewalConditionRepository.findByYearAndLastPaidAmount(
						customerPaidAccounts.size() + 1,
						customerAccountDTO.getIssueAmount().longValue());

			}
			else {
				renewalConditions = renewalConditionRepository.findByYearAndLastPaidAmount(
						customerPaidAccounts.size(), customerPaidAccounts.get(0).longValue());
			}

			// check if object is null
			if (ACMValidationUtils.isNullOrEmpty(renewalConditions)) {
				if (Boolean.TRUE.equals(customerAccountDTO.getCanTopup())) {
					renewalConditions = renewalConditionRepository.findByMaxYearAndLastPaidAmount(
							customerAccountDTO.getIssueAmount().longValue());
				}
				else {
					renewalConditions = renewalConditionRepository.findByMaxYearAndLastPaidAmount(
							customerPaidAccounts.get(0).longValue());
				}

				if (ACMValidationUtils.isNullOrEmpty(renewalConditions)) {

					// throw exception no renewal setting found
					throw new RenewalConditionSettingLoanException(
							new ExceptionResponseMessage(
									CommonErrorCode.RENEWAL_CONDITION_SETTING_NOT_FOUND,
									CommonExceptionsMessage.RENEWAL_CONDITION_SETTING_NOT_FOUND),
							CommonExceptionsMessage.RENEWAL_CONDITION_SETTING_NOT_FOUND);
				}
			}
			// set renewal condition Setting by customer
			RenewalConditionDTO renewalConditionDTO =
					mapper.map(renewalConditions.get(0), RenewalConditionDTO.class);
			renewalConditionLoanDTO.setRenewalConditionDTO(renewalConditionDTO);
			// case of Topup
			if (Boolean.TRUE.equals(customerAccountDTO.getCanTopup())) {
				renewalConditionLoanDTO
						.setLastLoanAmount(customerAccountDTO.getIssueAmount().longValue());

			}
			// case of renewal loan
			else {

				renewalConditionLoanDTO.setLastLoanAmount(customerPaidAccounts.get(0).longValue());
			}
			return renewalConditionLoanDTO;
		}
		else {
			return null;
		}
	}

}
