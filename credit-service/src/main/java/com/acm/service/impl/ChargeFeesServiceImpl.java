package com.acm.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.acm.client.UserClient;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.ChargeFeesRepository;
import com.acm.service.ChargeFeesService;
import com.acm.utils.dtos.ChargeFeesDTO;
import com.acm.utils.models.ChargeFees;
import com.acm.utils.models.QChargeFees;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * The Class ChargeFeesServiceImpl.
 */
@Service
public class ChargeFeesServiceImpl implements ChargeFeesService {
	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(ChargeFeesServiceImpl.class);

	/** The asset repository. */
	@Autowired
	private ChargeFeesRepository chargeFeesRepository;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The user client. */
	@Autowired
	private UserClient userClient;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ChargeFeesService#find(com.acm.utils.dtos.ChargeFeesDTO)
	 */
	@Override
	public List<ChargeFeesDTO> find(ChargeFeesDTO chargeFeesDTO) {

		Preconditions.checkNotNull(chargeFeesDTO, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		// init QAddress
		QChargeFees qChargeFees = QChargeFees.chargeFees;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();
		// find only enabled data
		predicate.and(qChargeFees.enabled.eq(Boolean.TRUE));

		// find by id
		if (!ACMValidationUtils.isNullOrEmpty(chargeFeesDTO.getId())) {
			predicate.and(qChargeFees.id.eq(chargeFeesDTO.getId()));
		}
		// find by idLoan
		if (!ACMValidationUtils.isNullOrEmpty(chargeFeesDTO.getIdLoanInstance())) {
			predicate.and(qChargeFees.loanInstance.id.eq(chargeFeesDTO.getIdLoanInstance()));
		}
		// find by idCollection
		if (!ACMValidationUtils.isNullOrEmpty(chargeFeesDTO.getIdCollectionInstance())) {
			predicate.and(
					qChargeFees.collectionInstance.id.eq(chargeFeesDTO.getIdCollectionInstance()));
		}
		// find by charged
		if (!ACMValidationUtils.isNullOrEmpty(chargeFeesDTO.getCharged())) {
			predicate.and(qChargeFees.charged.eq(chargeFeesDTO.getCharged()));
		}
		Iterable<ChargeFees> iterable = chargeFeesRepository.findAll(predicate);
		List<ChargeFees> chargeFees = new ArrayList<>();
		iterable.forEach(chargeFees::add);
		logger.info("{} : charged fees were found", chargeFees.size());

		List<ChargeFeesDTO> chargeFeesDTOs = new ArrayList<>();
		chargeFees.forEach(asset -> chargeFeesDTOs.add(mapper.map(asset, ChargeFeesDTO.class)));
		return chargeFeesDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ChargeFeesService#save(com.acm.utils.dtos.ChargeFeesDTO)
	 */
	@Override
	public ChargeFeesDTO save(ChargeFeesDTO chargeFeesDTO) {

		Preconditions.checkNotNull(chargeFeesDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		ChargeFees chargeFees = mapper.map(chargeFeesDTO, ChargeFees.class);
		if (ACMValidationUtils.isNullOrEmpty(chargeFeesDTO.getIdCollectionInstance())) {
			chargeFees.setCollectionInstance(null);
		}
		if (ACMValidationUtils.isNullOrEmpty(chargeFeesDTO.getIdLoanInstance())) {
			chargeFees.setLoanInstance(null);
		}
		CommonFunctions.mapperToSave(chargeFees, userClient, logger);
		ChargeFees newchargeFees = chargeFeesRepository.save(chargeFees);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, ChargeFees.class.getSimpleName());
		return mapper.map(newchargeFees, ChargeFeesDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ChargeFeesService#saveAll(java.util.List)
	 */
	@Override
	public List<ChargeFeesDTO> saveAll(List<ChargeFeesDTO> chargeFeesDTOs) {

		logger.info("Begin save all charge fees...");
		List<ChargeFees> ChargeFees = new ArrayList<>();
		List<ChargeFeesDTO> chargeFeesDTOResults = new ArrayList<>();
		for (ChargeFeesDTO chargeFeesDTO : chargeFeesDTOs) {
			chargeFeesDTO.setCharged(Boolean.TRUE);
			ChargeFees chargeFee = mapper.map(chargeFeesDTO, ChargeFees.class);
			if (ACMValidationUtils.isNullOrEmpty(chargeFeesDTO.getIdCollectionInstance())) {
				chargeFee.setCollectionInstance(null);
			}
			if (ACMValidationUtils.isNullOrEmpty(chargeFeesDTO.getIdLoanInstance())) {
				chargeFee.setLoanInstance(null);
			}
			CommonFunctions.mapperToSave(chargeFee, userClient, logger);
			ChargeFees.add(chargeFee);
		}
		if (!ACMValidationUtils.isNullOrEmpty(ChargeFees)) {
			chargeFeesRepository.saveAll(ChargeFees)
					.forEach(cf -> chargeFeesDTOResults.add(mapper.map(cf, ChargeFeesDTO.class)));
		}
		return chargeFeesDTOResults;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ChargeFeesService#save(java.lang.Long, com.acm.utils.dtos.ChargeFeesDTO)
	 */
	@Override
	public ChargeFeesDTO save(Long id, ChargeFeesDTO chargeFeesDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(chargeFeesDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Update asset with ID = {}", id);
		ChargeFees oldChargeFees = chargeFeesRepository.findById(id).orElse(null);

		// check if object is null
		if (oldChargeFees == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, ChargeFees.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + ChargeFees.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		// mapping new data with existing data
		mapper.map(chargeFeesDTO, oldChargeFees);
		if (ACMValidationUtils.isNullOrEmpty(chargeFeesDTO.getIdCollectionInstance())) {
			oldChargeFees.setCollectionInstance(null);
		}
		if (ACMValidationUtils.isNullOrEmpty(chargeFeesDTO.getIdLoanInstance())) {
			oldChargeFees.setLoanInstance(null);
		}
		CommonFunctions.mapperToUpdate(oldChargeFees, userClient, logger);

		// update & persist data in DB
		ChargeFees newChargeFee = chargeFeesRepository.save(oldChargeFees);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, ChargeFees.class.getSimpleName());
		return mapper.map(newChargeFee, ChargeFeesDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ChargeFeesService#delete(long)
	 */
	@Override
	public void delete(long id) {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.warn("delete by Id : {}", id);
		chargeFeesRepository.deleteById(id);
		logger.info(CommonLoggerMessage.SUCCESFULL_DELETE, ChargeFeesDTO.class.getSimpleName());

	}

}
