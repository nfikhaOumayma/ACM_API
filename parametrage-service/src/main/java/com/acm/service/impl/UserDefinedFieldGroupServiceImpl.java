/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Service;

import com.acm.client.TransversClient;
import com.acm.client.UserClient;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.UserDefinedFieldGroupRepository;
import com.acm.service.UserDefinedFieldGroupService;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.dtos.UserDefinedFieldGroupDTO;
import com.acm.utils.enums.SettingUDFCustomerType;
import com.acm.utils.models.QUserDefinedFieldGroup;
import com.acm.utils.models.UserDefinedFieldGroup;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link UserDefinedFieldGroupServiceImpl} Class Impl.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
@Service
public class UserDefinedFieldGroupServiceImpl implements UserDefinedFieldGroupService {

	/** The Constant logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(UserDefinedFieldGroupServiceImpl.class);

	/** The userDefinedFieldGroup repository. */
	@Autowired
	private UserDefinedFieldGroupRepository userDefinedFieldGroupRepository;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The Environment. */
	@Autowired
	private Environment environment;

	/** The user Client client. */
	@Autowired
	private UserClient userClient;

	/** The setting transvers client. */
	@Autowired
	private TransversClient settingTransversClient;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserDefinedFieldGroupService#find(java.lang.Long)
	 */
	@Override
	public UserDefinedFieldGroupDTO find(Long id) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find UserDefinedFieldGroup by ID : {}", id);
		UserDefinedFieldGroup userDefinedFieldGroup =
				userDefinedFieldGroupRepository.findById(id).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(userDefinedFieldGroup)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					UserDefinedFieldGroup.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("exception.message.not.found")
							+ UserDefinedFieldGroup.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		return mapper.map(userDefinedFieldGroup, UserDefinedFieldGroupDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserDefinedFieldGroupService#find(com.acm.utils.dtos.
	 * UserDefinedFieldGroupDTO)
	 */
	@Override
	public List<UserDefinedFieldGroupDTO> find(UserDefinedFieldGroupDTO userDefinedFieldGroupDTO) {

		Preconditions.checkNotNull(userDefinedFieldGroupDTO,
				CommonExceptionsMessage.EXCEPTIONS_ID_NULL);

		// init QUserDefinedFieldGroup
		QUserDefinedFieldGroup qUserDefinedFieldGroup =
				QUserDefinedFieldGroup.userDefinedFieldGroup;

		// init Predicate
		BooleanBuilder predicate = buildQuery(userDefinedFieldGroupDTO, qUserDefinedFieldGroup);
		if (predicate == null) {
			return new ArrayList<>();
		}
		// finding data
		Iterable<UserDefinedFieldGroup> iterable =
				userDefinedFieldGroupRepository.findAll(predicate);
		List<UserDefinedFieldGroup> userDefinedFieldGroups = new ArrayList<>();
		iterable.forEach(userDefinedFieldGroups::add);
		logger.info("{} : UserDefinedFieldGroup was founded", userDefinedFieldGroups.size());

		// mapping data
		List<UserDefinedFieldGroupDTO> userDefinedFieldGroupsDTOs = new ArrayList<>();
		userDefinedFieldGroups.forEach(userDefinedFieldGroup -> userDefinedFieldGroupsDTOs
				.add(mapper.map(userDefinedFieldGroup, UserDefinedFieldGroupDTO.class)));
		return userDefinedFieldGroupsDTOs;
	}

	/**
	 * Builds the query.
	 *
	 * @author HaythemBenizid
	 * @param userDefinedFieldGroupDTO the user defined field group DTO
	 * @param qUserDefinedFieldGroup the q user defined field group
	 * @return the boolean builder
	 */
	private BooleanBuilder buildQuery(UserDefinedFieldGroupDTO userDefinedFieldGroupDTO,
			QUserDefinedFieldGroup qUserDefinedFieldGroup) {

		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find by ID
		if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldGroupDTO.getId())) {
			predicate.and(qUserDefinedFieldGroup.id.eq(userDefinedFieldGroupDTO.getId()));
		}

		if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldGroupDTO.getIdUDGroupAbacus())) {
			if (userDefinedFieldGroupDTO.getIdUDGroupAbacus() == 0L) {
				predicate.and(qUserDefinedFieldGroup.idUDGroupAbacus.eq(0L));
			}
		}
		else {
			// find only enabled data
			predicate.and(qUserDefinedFieldGroup.enabled.eq(Boolean.TRUE));
			//predicate.and(qUserDefinedFieldGroup.idUDGroupAbacus.ne(0L));
		}

		// find by id UDGroup ABACUS
		if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldGroupDTO.getIdUDGroupAbacus())) {
			predicate.and(qUserDefinedFieldGroup.idUDGroupAbacus
					.eq(userDefinedFieldGroupDTO.getIdUDGroupAbacus()));
		}

		// find by loan data
		if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldGroupDTO.getLoanId())) {
			predicate.and(qUserDefinedFieldGroup.loanId.eq(userDefinedFieldGroupDTO.getLoanId()));
			// filter by ID product
			if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldGroupDTO.getProductId())) {
				predicate.and(qUserDefinedFieldGroup.productId
						.like("%" + userDefinedFieldGroupDTO.getProductId() + "%"));
			}
			else {
				return null;
			}
		}

		// find by category
		if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldGroupDTO.getCategory())) {
			predicate.and(qUserDefinedFieldGroup.category
					.like("%" + userDefinedFieldGroupDTO.getCategory() + "%"));
		}

		// find by customer data
		if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldGroupDTO.getCustomerId())) {
			predicate.and(
					qUserDefinedFieldGroup.customerId.eq(userDefinedFieldGroupDTO.getCustomerId()));
		}

		// find by customerType data
		if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldGroupDTO.getCustomerType())) {
			predicate.and(qUserDefinedFieldGroup.customerType
					.eq(userDefinedFieldGroupDTO.getCustomerType()));
		}

		// find by customerType label && customerType is NULL data
		if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldGroupDTO.getCustomerTypeLabel())
				&& ACMValidationUtils.isNullOrEmpty(userDefinedFieldGroupDTO.getCustomerType())) {
			List<Integer> ids =
					SettingUDFCustomerType.typeIds(userDefinedFieldGroupDTO.getCustomerTypeLabel());
			predicate.and(qUserDefinedFieldGroup.customerType.in(ids));
		}
		return predicate;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserDefinedFieldGroupService#save(com.acm.utils.dtos.
	 * UserDefinedFieldGroupDTO)
	 */
	@Override
	public UserDefinedFieldGroupDTO save(UserDefinedFieldGroupDTO userDefinedFieldGroupDTO,
			UserDTO userDTO) {

		Preconditions.checkNotNull(userDefinedFieldGroupDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		UserDefinedFieldGroup userDefinedFieldGroup =
				mapper.map(userDefinedFieldGroupDTO, UserDefinedFieldGroup.class);
		if (ACMValidationUtils.isNullOrEmpty(userDTO.getNom())) {
			userDTO = CommonFunctions.getConnectedUser(logger);
		}

		userDefinedFieldGroup.setInsertBy(userDTO.getFullName());
		userDefinedFieldGroup.setAcmVersion(0);
		userDefinedFieldGroup.setDateInsertion(new Date());
		userDefinedFieldGroup.setEnabled(userDefinedFieldGroupDTO.getEnabled());

		UserDefinedFieldGroup newUserDefinedFieldGroup =
				userDefinedFieldGroupRepository.save(userDefinedFieldGroup);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE,
				UserDefinedFieldGroup.class.getSimpleName());
		return mapper.map(newUserDefinedFieldGroup, UserDefinedFieldGroupDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserDefinedFieldGroupService#save(java.lang.Long,
	 * com.acm.utils.dtos.UserDefinedFieldGroupDTO)
	 */
	@Override
	public UserDefinedFieldGroupDTO save(Long id, UserDefinedFieldGroupDTO userDefinedFieldGroupDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(userDefinedFieldGroupDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Update UserDefinedFieldGroup with ID = {}", id);
		UserDefinedFieldGroup oldUserDefinedFieldGroup =
				userDefinedFieldGroupRepository.findById(id).orElse(null);

		// check if object is null
		if (oldUserDefinedFieldGroup == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					UserDefinedFieldGroup.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + UserDefinedFieldGroup.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		// mapping new data with existing data (oldUserDefinedFieldGroup)
		mapper.map(userDefinedFieldGroupDTO, oldUserDefinedFieldGroup);
		CommonFunctions.mapperToUpdate(oldUserDefinedFieldGroup, userClient, logger);

		// update & persist data in DB
		UserDefinedFieldGroup newUserDefinedFieldGroup =
				userDefinedFieldGroupRepository.save(oldUserDefinedFieldGroup);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE,
				UserDefinedFieldGroup.class.getSimpleName());
		return mapper.map(newUserDefinedFieldGroup, UserDefinedFieldGroupDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserDefinedFieldGroupService#loadSettingFromAbacus()
	 */
	@Override
	public void loadSettingFromAbacus(UserDTO userDTO) throws ResourcesNotFoundException {

		// load list UDF group from ABACUS DB
		List<UserDefinedFieldGroupDTO> userDefinedFieldGroupDTOs =
				settingTransversClient.findUserDefinedFieldGroup();
		for (UserDefinedFieldGroupDTO userDefinedFieldGroupDTO : userDefinedFieldGroupDTOs) {
			logger.debug("{}", userDefinedFieldGroupDTO);
			// check if UDF group exist in ACM DB
			List<UserDefinedFieldGroup> userDefinedFieldGroups =
					userDefinedFieldGroupRepository.findByIdUDGroupAbacusAndEnabled(
							userDefinedFieldGroupDTO.getIdUDGroupAbacus(), Boolean.TRUE);
			// save or update returned data
			if (ACMValidationUtils.isNullOrEmpty(userDefinedFieldGroups)) {
				// Save new entry
				save(userDefinedFieldGroupDTO, userDTO);
			}
			else {
				logger.debug("{}", userDefinedFieldGroups.get(0));
				// setting id UDF group
				userDefinedFieldGroupDTO.setId(userDefinedFieldGroups.get(0).getId());
				// Update existing UDF Group in ACM DB
				save(userDefinedFieldGroups.get(0).getId(), userDefinedFieldGroupDTO);
			}
		}
		logger.info("############ UserDefinedFieldGroup :: Processing DONE ############");
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.UserDefinedFieldGroupService#findByIds(java.util.List)
	 */
	@Override
	public List<UserDefinedFieldGroupDTO> findByIds(List<Long> ids) {

		Preconditions.checkNotNull(ids, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find UserDefinedFieldGroup by IDs : {}", ids);
		List<UserDefinedFieldGroup> userDefinedFieldGroups =
				userDefinedFieldGroupRepository.findByIdInAndEnabled(ids, Boolean.TRUE);

		return userDefinedFieldGroups.stream().map(userDefinedFieldGroup -> mapper
				.map(userDefinedFieldGroup, UserDefinedFieldGroupDTO.class))
				.collect(Collectors.toList());
	}

}
