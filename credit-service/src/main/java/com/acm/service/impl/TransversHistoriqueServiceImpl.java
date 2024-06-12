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
import org.springframework.stereotype.Service;

import com.acm.client.UserClient;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.TransversHistoriqueRepository;
import com.acm.service.TransversHistoriqueService;
import com.acm.utils.dtos.TransversHistoriqueDTO;
import com.acm.utils.models.QTransversHistorique;
import com.acm.utils.models.TransversHistorique;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link TransversHistoriqueServiceImpl} class.
 *
 * @author MoezMhiri
 * @since 1.0.12
 */
@Service
public class TransversHistoriqueServiceImpl implements TransversHistoriqueService {

	/** The Constant logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(TransversHistoriqueServiceImpl.class);

	/** The third party historique repository. */
	@Autowired
	private TransversHistoriqueRepository transversHistoriqueRepository;

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
	 * @see com.acm.service.TransversHistoriqueService#find(java.lang.Long)
	 */
	@Override
	public TransversHistoriqueDTO find(Long id) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find 3-RD Party Historique by ID : {}", id);
		TransversHistorique transversHistorique =
				transversHistoriqueRepository.findById(id).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(transversHistorique)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					TransversHistorique.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("exception.message.not.found")
							+ TransversHistorique.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		return mapper.map(transversHistorique, TransversHistoriqueDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.TransversHistoriqueService#find(com.acm.utils.dtos.
	 * TransversHistoriqueDTO)
	 */
	@Override
	public List<TransversHistoriqueDTO> find(TransversHistoriqueDTO transversHistoriqueDTO) {

		Preconditions.checkNotNull(transversHistoriqueDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);

		// init QTransversHistorique
		QTransversHistorique qTransversHistorique = QTransversHistorique.transversHistorique;

		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find only enabled data
		predicate.and(qTransversHistorique.enabled.eq(Boolean.TRUE));

		// find by ObjectValue (Table Name)
		if (!ACMValidationUtils.isNullOrEmpty(transversHistoriqueDTO.getObjectValue())) {
			predicate.and(
					qTransversHistorique.objectValue.eq(transversHistoriqueDTO.getObjectValue()));
		}

		// find by Methode (GEt -POST - PUT)
		if (!ACMValidationUtils.isNullOrEmpty(transversHistoriqueDTO.getMethode())) {
			predicate.and(qTransversHistorique.methode.eq(transversHistoriqueDTO.getMethode()));
		}

		// find by Uri
		if (!ACMValidationUtils.isNullOrEmpty(transversHistoriqueDTO.getUri())) {
			predicate.and(qTransversHistorique.uri.eq(transversHistoriqueDTO.getUri()));
		}

		// find by Status
		if (!ACMValidationUtils.isNullOrEmpty(transversHistoriqueDTO.getStatus())) {
			predicate.and(qTransversHistorique.status.eq(transversHistoriqueDTO.getStatus()));
		}

		// QueryDSL using springDATA
		Iterable<TransversHistorique> iterable = transversHistoriqueRepository.findAll(predicate,
				new Sort(Sort.Direction.DESC, "dateInsertion"));
		List<TransversHistorique> transversHistoriques = new ArrayList<>();
		iterable.forEach(transversHistoriques::add);
		logger.info("{} : Transvers history was founded", transversHistoriques.size());

		// mapping returned list
		List<TransversHistoriqueDTO> transversHistoriqueDTOs = new ArrayList<>();
		transversHistoriques.forEach(transversHistorique -> transversHistoriqueDTOs
				.add(mapper.map(transversHistorique, TransversHistoriqueDTO.class)));

		logger.info("Returning founded data ...");
		return transversHistoriqueDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.TransversHistoriqueService#save(com.acm.utils.dtos.
	 * TransversHistoriqueDTO)
	 */
	@Override
	public TransversHistoriqueDTO save(TransversHistoriqueDTO transversHistoriqueDTO) {

		Preconditions.checkNotNull(transversHistoriqueDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		TransversHistorique transversHistorique =
				mapper.map(transversHistoriqueDTO, TransversHistorique.class);
		CommonFunctions.mapperToSave(transversHistorique, userClient, logger);
		TransversHistorique newTransversHistorique =
				transversHistoriqueRepository.save(transversHistorique);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE,
				TransversHistorique.class.getSimpleName());
		return mapper.map(newTransversHistorique, TransversHistoriqueDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.TransversHistoriqueService#save(java.lang.Long,
	 * com.acm.utils.dtos.TransversHistoriqueDTO)
	 */
	@Override
	public TransversHistoriqueDTO save(Long id, TransversHistoriqueDTO transversHistoriqueDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(transversHistoriqueDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Update TransversHistorique with ID = {}", id);
		TransversHistorique oldTransversHistorique =
				transversHistoriqueRepository.findById(id).orElse(null);

		// check if object is null
		if (oldTransversHistorique == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					TransversHistorique.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + TransversHistorique.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		// mapping new data with existing data (oldTransversHistorique)
		mapper.map(transversHistoriqueDTO, oldTransversHistorique);
		CommonFunctions.mapperToUpdate(oldTransversHistorique, userClient, logger);

		// update & persist data in DB
		TransversHistorique newTransversHistorique =
				transversHistoriqueRepository.save(oldTransversHistorique);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE,
				TransversHistorique.class.getSimpleName());
		return mapper.map(newTransversHistorique, TransversHistoriqueDTO.class);
	}

}
