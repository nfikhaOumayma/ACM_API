package com.acm.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.AcmTemplateSMSRepository;
import com.acm.service.AcmTemplateSMSService;
import com.acm.utils.dtos.AcmTemplateSMSDTO;
import com.acm.utils.models.AcmTemplateSMS;
import com.acm.utils.models.QAcmTemplateSMS;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * The Class AcmTemplateSMSServiceImpl.
 */
@Service
public class AcmTemplateSMSServiceImpl implements AcmTemplateSMSService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(AcmTemplateSMSServiceImpl.class);

	/** The acm template SMS repository. */
	@Autowired
	private AcmTemplateSMSRepository acmTemplateSMSRepository;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmTemplateSMSService#find(com.acm.utils.dtos.AcmTemplateSMSDTO)
	 */
	@Override
	public List<AcmTemplateSMSDTO> find(AcmTemplateSMSDTO acmTemplateSMSDTO) {

		// init QSettingDocumentProduct
		QAcmTemplateSMS qAcmTemplateSMS = QAcmTemplateSMS.acmTemplateSMS;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		if (!ACMValidationUtils.isNullOrEmpty(acmTemplateSMSDTO.getCategory())) {
			predicate.and(qAcmTemplateSMS.category.eq(acmTemplateSMSDTO.getCategory()));
		}
		// QueryDSL using springDATA

		Iterable<AcmTemplateSMS> iterable = acmTemplateSMSRepository.findAll(predicate);
		List<AcmTemplateSMS> AcmTemplateSMSs = new ArrayList<>();
		iterable.forEach(AcmTemplateSMSs::add);
		logger.info("{} : Template SMS was founded", AcmTemplateSMSs.size());

		// mapping returned list
		List<AcmTemplateSMSDTO> acmTemplateSMSDTOs = new ArrayList<>();
		AcmTemplateSMSs.forEach(AcmTemplateSMS -> acmTemplateSMSDTOs
				.add(mapper.map(AcmTemplateSMS, AcmTemplateSMSDTO.class)));

		logger.info("Returning founded data ...");
		return acmTemplateSMSDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmTemplateSMSService#findByCode(com.acm.utils.dtos.AcmTemplateSMSDTO)
	 */
	@Override
	public AcmTemplateSMSDTO findByCode(AcmTemplateSMSDTO acmTemplateSMSDTO)
			throws ResourcesNotFoundException {

		// init QSettingDocumentProduct
		QAcmTemplateSMS qAcmTemplateSMS = QAcmTemplateSMS.acmTemplateSMS;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		if (!ACMValidationUtils.isNullOrEmpty(acmTemplateSMSDTO.getCodeSMSEvent())) {
			predicate.and(qAcmTemplateSMS.codeSMSEvent.eq(acmTemplateSMSDTO.getCodeSMSEvent()));
		}

		// QueryDSL using springDATA
		Iterable<AcmTemplateSMS> iterable = acmTemplateSMSRepository.findAll(predicate);
		List<AcmTemplateSMS> AcmTemplateSMSs = new ArrayList<>();
		iterable.forEach(AcmTemplateSMSs::add);
		logger.info("{} : Template SMS was founded", AcmTemplateSMSs.size());

		// mapping returned list
		List<AcmTemplateSMSDTO> acmTemplateSMSDTOs = new ArrayList<>();
		AcmTemplateSMSs.forEach(AcmTemplateSMS -> acmTemplateSMSDTOs
				.add(mapper.map(AcmTemplateSMS, AcmTemplateSMSDTO.class)));

		logger.info("Returning founded data ...");
		return acmTemplateSMSDTOs.get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmTemplateSMSService#save(com.acm.utils.dtos.AcmTemplateSMSDTO)
	 */
	@Override
	public AcmTemplateSMSDTO save(AcmTemplateSMSDTO acmTemplateSMSDTO) {

		Preconditions.checkNotNull(acmTemplateSMSDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		AcmTemplateSMS acmTemplateSMS = mapper.map(acmTemplateSMSDTO, AcmTemplateSMS.class);

		AcmTemplateSMS newacmTemplateSMS = acmTemplateSMSRepository.save(acmTemplateSMS);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, AcmTemplateSMS.class.getSimpleName());
		return mapper.map(newacmTemplateSMS, AcmTemplateSMSDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.AcmTemplateSMSService#updateMessageBody(com.acm.utils.dtos.AcmTemplateSMSDTO)
	 */
	@Override
	public AcmTemplateSMSDTO updateMessageBody(AcmTemplateSMSDTO acmTemplateSMSDTO) {

		Preconditions.checkNotNull(acmTemplateSMSDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		AcmTemplateSMS acmTemplateSMS = mapper.map(acmTemplateSMSDTO, AcmTemplateSMS.class);
		AcmTemplateSMS newacmTemplateSMS = acmTemplateSMSRepository
				.findById(acmTemplateSMS.getIdAcmTemplateSMS()).orElse(null);
		newacmTemplateSMS.setMessageBody(acmTemplateSMSDTO.getMessageBody());
		newacmTemplateSMS.setCategory(acmTemplateSMSDTO.getCategory());
		newacmTemplateSMS.setCodeSMSEvent(acmTemplateSMSDTO.getCodeSMSEvent());
		AcmTemplateSMS NewacmTemplateSMS = acmTemplateSMSRepository.save(newacmTemplateSMS);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, AcmTemplateSMS.class.getSimpleName());
		return mapper.map(NewacmTemplateSMS, AcmTemplateSMSDTO.class);
	}

}
