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
import com.acm.constants.common.CommonErrorCode;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.JournalEntryException;
import com.acm.exceptions.type.JournalEntryWorkflowStepException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.SettingJournalEntryTypeRepository;
import com.acm.service.SettingJournalEntryTypeService;
import com.acm.utils.dtos.SettingJournalEntryTypeDTO;
import com.acm.utils.models.QSettingJournalEntryType;
import com.acm.utils.models.SettingJournalEntryType;
import com.acm.utils.models.SettingMotifRejets;
import com.acm.utils.validation.ACMValidationUtils;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link SettingJournalEntryTypeServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 0.2.0
 */
@Service
public class SettingJournalEntryTypeServiceImpl implements SettingJournalEntryTypeService {

	/** The Constant logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(SettingJournalEntryTypeServiceImpl.class);

	/** The settingMotifRejets repository. */
	@Autowired
	private SettingJournalEntryTypeRepository settingJournalEntryRepository;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The user Client client. */
	@Autowired
	private UserClient userClient;

	/* (non-Javadoc)
	 * @see com.acm.service.SettingJournalEntryTypeService#find(com.acm.utils.dtos.SettingJournalEntryTypeDTO)
	 */
	@Override
	public List<SettingJournalEntryTypeDTO> find(
			SettingJournalEntryTypeDTO settingJournalEntryDTO) {

		// init QSettingMotifRejets
		QSettingJournalEntryType qSettingJournalEntry =
				QSettingJournalEntryType.settingJournalEntry;

		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find by enabled
		if (!ACMValidationUtils.isNullOrEmpty(settingJournalEntryDTO.getEnabled())) {
			predicate.and(qSettingJournalEntry.enabled.eq(settingJournalEntryDTO.getEnabled()));
		}

		// find by id
		if (!ACMValidationUtils.isNullOrEmpty(settingJournalEntryDTO.getId())) {
			predicate.and(qSettingJournalEntry.id.eq(settingJournalEntryDTO.getId()));
		}

		// QueryDSL using springDATA
		Iterable<SettingJournalEntryType> iterable =
				settingJournalEntryRepository.findAll(predicate);
		List<SettingJournalEntryType> settingJournalEntrys = new ArrayList<>();
		iterable.forEach(settingJournalEntrys::add);
		logger.info("{} : setting Motif Rejets was founded", settingJournalEntrys.size());

		// mapping returned list
		List<SettingJournalEntryTypeDTO> settingJournalEntryDTOs = new ArrayList<>();
		settingJournalEntrys.forEach(item -> settingJournalEntryDTOs
				.add(mapper.map(item, SettingJournalEntryTypeDTO.class)));

		logger.info("Returning founded data ...");
		return settingJournalEntryDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingJournalEntryTypeService#save(com.acm.utils.dtos.
	 * SettingJournalEntryTypeDTO)
	 */
	@Override
	public SettingJournalEntryTypeDTO save(SettingJournalEntryTypeDTO settingJournalEntryDTO) {

		SettingJournalEntryType settingJournalEntry =
				mapper.map(settingJournalEntryDTO, SettingJournalEntryType.class);
		CommonFunctions.mapperToSave(settingJournalEntry, userClient, logger);

		return mapper.map(settingJournalEntryRepository.save(settingJournalEntry),
				SettingJournalEntryTypeDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingJournalEntryTypeService#find()
	 */
	@Override
	public List<SettingJournalEntryTypeDTO> find() {

		List<SettingJournalEntryType> settingJournalEntrys =
				settingJournalEntryRepository.findAll();
		List<SettingJournalEntryTypeDTO> settingJournalEntryDTOs = new ArrayList<>();
		settingJournalEntrys.forEach(settingJournalEntry -> settingJournalEntryDTOs
				.add(mapper.map(settingJournalEntry, SettingJournalEntryTypeDTO.class)));
		return settingJournalEntryDTOs;
	}

	/**
	 * /* (non-Javadoc).
	 *
	 * @param id the id
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws JournalEntryWorkflowStepException the journal entry workflow step exception
	 * @see com.acm.service.SettingMotifRejetsService#delete(java.lang.Long)
	 */
	@Override
	public void delete(Long id)
			throws ResourcesNotFoundException, JournalEntryWorkflowStepException {

		// calculate number of occurrence of journal entry type in workflow step
		long nb = settingJournalEntryRepository.findJournalEntryTypesInWorkflowStep(id);

		if (nb > 0) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_DELETING_JOURNAL_ENTRY_TYPE);
			throw new JournalEntryWorkflowStepException(
					"Cannot delete Journal Entry. It is already used.");
		}
		// delete old Motif Rejet type
		settingJournalEntryRepository.deleteById(id);
		logger.info(CommonLoggerMessage.SUCCESFULL_DELETE,
				SettingMotifRejets.class.getSimpleName());
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingJournalEntryTypeService#update(com.acm.utils.dtos.
	 * SettingJournalEntryTypeDTO)
	 */
	@Override
	public SettingJournalEntryTypeDTO update(SettingJournalEntryTypeDTO settingJournalEntryDTO)
			throws JournalEntryException {

		SettingJournalEntryType settingJournalEntry =
				mapper.map(settingJournalEntryDTO, SettingJournalEntryType.class);

		CommonFunctions.mapperToUpdate(settingJournalEntry, userClient, logger);

		if (settingJournalEntryRepository
				.findJournalEntryTypesInCarrierTbl(settingJournalEntry.getId()) > 0) {
			throw new JournalEntryException(CommonErrorCode.JOURNAL_ENTRY_EXCEPTION,
					"Please check the journal entry (in use)");
		}

		return mapper.map(settingJournalEntryRepository.save(settingJournalEntry),
				SettingJournalEntryTypeDTO.class);
	}
}
