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
import com.acm.repository.ItemNoteRepository;
import com.acm.service.ItemNoteService;
import com.acm.utils.dtos.ItemNoteDTO;
import com.acm.utils.models.AcmCollateral;
import com.acm.utils.models.ItemNote;
import com.acm.utils.models.QItemNote;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link ItemNoteServiceImpl } class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
@Service
public class ItemNoteServiceImpl implements ItemNoteService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(ItemNoteServiceImpl.class);

	/** The collection note repository. */
	@Autowired
	private ItemNoteRepository itemNoteRepository;
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
	public List<ItemNoteDTO> find(ItemNoteDTO itemNoteDTO) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(itemNoteDTO, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		// if id collection is Null
		if (ACMValidationUtils.isNullOrEmpty(itemNoteDTO.getItemId())) {
			return new ArrayList<>();
		}

		// init QItemNote
		QItemNote qItemNote = QItemNote.itemNote;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();
		// find only enabled data
		predicate.and(qItemNote.enabled.eq(Boolean.TRUE));

		// find by collectionId
		if (!ACMValidationUtils.isNullOrEmpty(itemNoteDTO.getItemId())) {
			predicate.and(qItemNote.itemId.eq(itemNoteDTO.getItemId()));
		}

		Iterable<ItemNote> iterable = itemNoteRepository.findAll(predicate);
		List<ItemNote> ItemNotes = new ArrayList<>();
		iterable.forEach(ItemNotes::add);
		logger.info("{} : Address was founded", ItemNotes.size());

		List<ItemNoteDTO> collectionNoteDTOs = new ArrayList<>();
		ItemNotes.forEach(note -> collectionNoteDTOs.add(mapper.map(note, ItemNoteDTO.class)));
		return collectionNoteDTOs;

	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CollectionNoteService#save(com.acm.utils.dtos.CollectionNoteDTO)
	 */
	@Override
	public ItemNoteDTO save(ItemNoteDTO itemNoteDTO) {

		Preconditions.checkNotNull(itemNoteDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		ItemNote itemNote = mapper.map(itemNoteDTO, ItemNote.class);

		CommonFunctions.mapperToSave(itemNote, userClient, logger);
		ItemNote result = itemNoteRepository.save(itemNote);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, AcmCollateral.class.getSimpleName());
		return mapper.map(result, ItemNoteDTO.class);
	}

}
