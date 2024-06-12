/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.stereotype.Service;

import com.acm.client.UserClient;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.CollectionInstanceRepository;
import com.acm.service.CollectionInstanceService;
import com.acm.utils.dtos.CollectionInstanceDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.models.AcmCollection;
import com.acm.utils.models.AcmThirdParty;
import com.acm.utils.models.CollectionInstance;
import com.acm.utils.models.QCollectionInstance;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * The Class CollectionInstanceServiceImpl.
 */
@Service
public class CollectionInstanceServiceImpl implements CollectionInstanceService {
	/** The Constant logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(CollectionInstanceServiceImpl.class);

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The collection instance repository. */
	@Autowired
	private CollectionInstanceRepository collectionInstanceRepository;

	/** The user client. */
	@Autowired
	private UserClient userClient;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CollectionInstanceService#find(com.acm.utils.dtos.CollectionInstanceDTO)
	 */
	@Override
	public List<CollectionInstanceDTO> find(CollectionInstanceDTO collectionInstanceDTO) {

		Preconditions.checkNotNull(collectionInstanceDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// init QCollectionInstance
		QCollectionInstance qCollectionInstance = QCollectionInstance.collectionInstance;

		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// only enabled row
		predicate.and(qCollectionInstance.enabled.eq(Boolean.TRUE));

		// find by collection
		if (collectionInstanceDTO.getIdCollection() != null) {
			predicate.and(qCollectionInstance.collection
					.eq(new AcmCollection(collectionInstanceDTO.getIdCollection())));
		}
		else {
			return new ArrayList<>();
		}
		// QueryDSL using springDATA
		Iterable<CollectionInstance> iterable = collectionInstanceRepository.findAll(predicate,
				Sort.by(Direction.ASC, "orderEtapeProcess"));
		List<CollectionInstance> collectionInstances = new ArrayList<>();
		iterable.forEach(collectionInstances::add);

		// mapping returned list
		List<CollectionInstanceDTO> collectionInstanceDTOs = new ArrayList<>();
		collectionInstances.forEach(collectionInstance -> collectionInstanceDTOs
				.add(mapper.map(collectionInstance, CollectionInstanceDTO.class)));

		logger.info("Returning founded data");
		return collectionInstanceDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CollectionInstanceService#save(com.acm.utils.dtos.CollectionInstanceDTO)
	 */
	@Override
	public CollectionInstanceDTO save(CollectionInstanceDTO collectionInstanceDTO) {

		// TODO collection process

		Preconditions.checkNotNull(collectionInstanceDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		CollectionInstance collectionInstance =
				mapper.map(collectionInstanceDTO, CollectionInstance.class);

		UserDTO userDTO = CommonFunctions.getConnectedUser(logger);
		collectionInstance.setInsertBy(userDTO.getFullName());
		collectionInstance.setAcmVersion(0);
		collectionInstance.setDateInsertion(new Date());
		CollectionInstance newCollectionInstance =
				collectionInstanceRepository.save(collectionInstance);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE,
				CollectionInstance.class.getSimpleName());
		return mapper.map(newCollectionInstance, CollectionInstanceDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CollectionInstanceService#save(java.lang.Long,
	 * com.acm.utils.dtos.CollectionInstanceDTO)
	 */
	@Override
	public CollectionInstanceDTO save(Long id, CollectionInstanceDTO collectionInstanceDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(collectionInstanceDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Update CollectionInstance with ID = {}", id);
		CollectionInstance oldCollectionInstance =
				collectionInstanceRepository.findById(id).orElse(null);

		// check if object is null
		if (oldCollectionInstance == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					CollectionInstance.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + CollectionInstance.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}

		// set the new third party list of this collection
		oldCollectionInstance.setThirdParties(new HashSet<>());
		oldCollectionInstance.getThirdParties().forEach(thirdPartyDTO -> {
			oldCollectionInstance.getThirdParties()
					.add(mapper.map(thirdPartyDTO, AcmThirdParty.class));
		});
		CommonFunctions.mapperToUpdate(oldCollectionInstance, userClient, logger);
		// mapping new data with existing data (oldCollectionInstance)
		mapper.map(collectionInstanceDTO, oldCollectionInstance);
		// update & persist data in DB
		CollectionInstance newCollectionInstance =
				collectionInstanceRepository.save(oldCollectionInstance);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE,
				CollectionInstance.class.getSimpleName());
		return mapper.map(newCollectionInstance, CollectionInstanceDTO.class);
	}

}
