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

import com.acm.client.UserClient;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.CollectionNoteRepository;
import com.acm.service.CollectionNoteService;
import com.acm.utils.dtos.CollectionNoteDTO;
import com.acm.utils.models.AcmCollateral;
import com.acm.utils.models.CollectionNote;
import com.acm.utils.models.QCollectionNote;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link CollectionNoteServiceImpl } class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
@Service
public class CollectionNoteServiceImpl implements CollectionNoteService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(CollectionNoteServiceImpl.class);

	/** The collection note repository. */
	@Autowired
	private CollectionNoteRepository collectionNoteRepository;
	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;
	/** The user Client client. */
	@Autowired
	private UserClient userClient;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CollectionNoteService#find(com.acm.utils.dtos.CollectionNoteDTO)
	 */
	@Override
	public List<CollectionNoteDTO> find(CollectionNoteDTO collectionNoteDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(collectionNoteDTO, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		// if id collection is Null
		if (ACMValidationUtils.isNullOrEmpty(collectionNoteDTO.getCollectionId())) {
			return new ArrayList<>();
		}

		// init QAddress
		QCollectionNote qCollectionNote = QCollectionNote.collectionNote;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();
		// find only enabled data
		predicate.and(qCollectionNote.enabled.eq(Boolean.TRUE));

		// find by collectionId
		if (!ACMValidationUtils.isNullOrEmpty(collectionNoteDTO.getCollectionId())) {
			predicate.and(qCollectionNote.collectionId.eq(collectionNoteDTO.getCollectionId()));
		}

		Iterable<CollectionNote> iterable = collectionNoteRepository.findAll(predicate);
		List<CollectionNote> collectionNotes = new ArrayList<>();
		iterable.forEach(collectionNotes::add);
		logger.info("{} : Address was founded", collectionNotes.size());

		List<CollectionNoteDTO> collectionNoteDTOs = new ArrayList<>();
		collectionNotes
				.forEach(note -> collectionNoteDTOs.add(mapper.map(note, CollectionNoteDTO.class)));
		return collectionNoteDTOs;

	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CollectionNoteService#save(com.acm.utils.dtos.CollectionNoteDTO)
	 */
	@Override
	public CollectionNoteDTO save(CollectionNoteDTO collectionNoteDTO) {

		Preconditions.checkNotNull(collectionNoteDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		CollectionNote collectionNote = mapper.map(collectionNoteDTO, CollectionNote.class);

		CommonFunctions.mapperToSave(collectionNote, userClient, logger);
		CollectionNote result = collectionNoteRepository.save(collectionNote);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, AcmCollateral.class.getSimpleName());
		return mapper.map(result, CollectionNoteDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CollectionNoteService#save(com.acm.utils.dtos.CollectionNoteDTO,
	 * java.lang.String)
	 */
	@Override
	public CollectionNoteDTO save(CollectionNoteDTO collectionNoteDTO, String insertBy) {

		Preconditions.checkNotNull(collectionNoteDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		CollectionNote collectionNote = mapper.map(collectionNoteDTO, CollectionNote.class);

		CommonFunctions.mapperToSave(collectionNote, userClient, logger);
		collectionNote.setInsertBy(insertBy);
		CollectionNote result = collectionNoteRepository.save(collectionNote);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, AcmCollateral.class.getSimpleName());
		return mapper.map(result, CollectionNoteDTO.class);
	}

}
