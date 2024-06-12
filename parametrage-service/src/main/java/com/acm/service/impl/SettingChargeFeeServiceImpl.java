package com.acm.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Service;

import com.acm.client.UserClient;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.SettingChargeFeeRepository;
import com.acm.service.SettingChargeFeeService;
import com.acm.utils.dtos.SettingChargeFeeDTO;
import com.acm.utils.models.QSettingChargeFee;
import com.acm.utils.models.SettingChargeFee;
import com.acm.utils.models.SettingDocumentProduct;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * The Class SettingChargeFeeServiceImpl.
 */
@Service
public class SettingChargeFeeServiceImpl implements SettingChargeFeeService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(SettingChargeFeeServiceImpl.class);

	/** The setting charge fee repository. */
	@Autowired
	private SettingChargeFeeRepository settingChargeFeeRepository;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The environment. */
	@Autowired
	private Environment environment;

	/** The user client. */
	@Autowired
	private UserClient userClient;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingChargeFeeService#find(com.acm.utils.dtos.SettingChargeFeeDTO)
	 */
	@Override
	public List<SettingChargeFeeDTO> find(SettingChargeFeeDTO settingChargeFeeDTO) {

		QSettingChargeFee qSettingChargeFee = QSettingChargeFee.settingChargeFee;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find by Id
		if (!ACMValidationUtils.isNullOrEmpty(settingChargeFeeDTO.getId())) {
			predicate.and(qSettingChargeFee.id.eq(settingChargeFeeDTO.getId()));
		}
		// find by CUFeeId
		if (!ACMValidationUtils.isNullOrEmpty(settingChargeFeeDTO.getCufeeId())) {
			predicate.and(qSettingChargeFee.cufeeId.eq(settingChargeFeeDTO.getCufeeId()));
		}
		// find by Value
		if (!ACMValidationUtils.isNullOrEmpty(settingChargeFeeDTO.getValue())) {
			predicate.and(qSettingChargeFee.value.eq(settingChargeFeeDTO.getValue()));
		}
		// find by Amount
		if (!ACMValidationUtils.isNullOrEmpty(settingChargeFeeDTO.getAmount())) {
			predicate.and(qSettingChargeFee.amount.eq(settingChargeFeeDTO.getAmount()));
		}
		// find by Label
		if (!ACMValidationUtils.isNullOrEmpty(settingChargeFeeDTO.getLabel())) {
			predicate.and(qSettingChargeFee.label.eq(settingChargeFeeDTO.getLabel()));
		}
		// find by Code
		if (!ACMValidationUtils.isNullOrEmpty(settingChargeFeeDTO.getCode())) {
			predicate.and(qSettingChargeFee.code.eq(settingChargeFeeDTO.getCode()));
		}
		// find by IdLoanExtern
		if (!ACMValidationUtils.isNullOrEmpty(settingChargeFeeDTO.getIdLoanExtern())) {
			predicate.and(qSettingChargeFee.idLoanExtern.eq(settingChargeFeeDTO.getIdLoanExtern()));
		}
		// find by IdCollection
		if (!ACMValidationUtils.isNullOrEmpty(settingChargeFeeDTO.getIdCollection())) {
			predicate.and(qSettingChargeFee.idCollection.eq(settingChargeFeeDTO.getIdCollection()));
		}
		// QueryDSL using springDATA
		Iterable<SettingChargeFee> iterable = settingChargeFeeRepository.findAll(predicate);
		List<SettingChargeFee> settingChargeFees = new ArrayList<>();
		iterable.forEach(settingChargeFees::add);
		logger.info("{} : Setting Charge fee was found", settingChargeFees.size());

		// mapping returned list
		List<SettingChargeFeeDTO> settingChargeFeeDTOs = new ArrayList<>();
		settingChargeFees.forEach(settingChargeFee -> settingChargeFeeDTOs
				.add(mapper.map(settingChargeFee, SettingChargeFeeDTO.class)));

		logger.info("Returning found data ...");
		return settingChargeFeeDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingChargeFeeService#find(java.lang.Long)
	 */
	@Override
	public SettingChargeFeeDTO find(Long id) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find settingChargeFee by ID : {}", id);
		SettingChargeFee settingChargeFee = settingChargeFeeRepository.findById(id).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(settingChargeFee)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					SettingChargeFee.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("exception.message.not.found")
							+ SettingDocumentProduct.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		return mapper.map(settingChargeFee, SettingChargeFeeDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingChargeFeeService#find()
	 */
	@Override
	public List<SettingChargeFeeDTO> find() {

		List<SettingChargeFee> settingChargeFees = settingChargeFeeRepository.findAll();
		List<SettingChargeFeeDTO> settingChargeFeeDTOs = new ArrayList<>();
		settingChargeFees.forEach(settingChargeFee -> settingChargeFeeDTOs
				.add(mapper.map(settingChargeFee, SettingChargeFeeDTO.class)));
		return settingChargeFeeDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingChargeFeeService#save(java.lang.Long,
	 * com.acm.utils.dtos.SettingChargeFeeDTO)
	 */
	@Override
	public SettingChargeFeeDTO save(Long id, SettingChargeFeeDTO settingChargeFeeDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(settingChargeFeeDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Update SettingChargeFee with ID = {}", id);
		SettingChargeFee oldSettingChargeFee = settingChargeFeeRepository.findById(id).orElse(null);

		// check if object is null
		if (oldSettingChargeFee == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					SettingChargeFee.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + SettingChargeFee.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}

		// mapping new data with existing data (oldSettingDocumentProduct)
		mapper.map(settingChargeFeeDTO, oldSettingChargeFee);
		CommonFunctions.mapperToUpdate(oldSettingChargeFee, userClient, logger);

		// update & persist data in DB
		SettingChargeFee newSettingChargeFee = settingChargeFeeRepository.save(oldSettingChargeFee);
		SettingChargeFeeDTO newSettingChargeFeeDTO =
				mapper.map(newSettingChargeFee, SettingChargeFeeDTO.class);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, SettingChargeFee.class.getSimpleName());
		return newSettingChargeFeeDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingChargeFeeService#save(com.acm.utils.dtos.SettingChargeFeeDTO)
	 */
	@Override
	public SettingChargeFeeDTO save(SettingChargeFeeDTO settingChargeFeeDTO) {

		Preconditions.checkNotNull(settingChargeFeeDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		SettingChargeFee settingChargeFee = mapper.map(settingChargeFeeDTO, SettingChargeFee.class);

		CommonFunctions.mapperToSave(settingChargeFee, userClient, logger);
		settingChargeFee.setEnabled(Boolean.FALSE);
		settingChargeFeeRepository.save(settingChargeFee);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, SettingChargeFee.class.getSimpleName());
		return mapper.map(settingChargeFee, SettingChargeFeeDTO.class);
	}

}
