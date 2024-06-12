/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.acm.client.UserClient;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.SettingJournalEnteriesRepository;
import com.acm.repository.SettingJournalEntryTypeRepository;
import com.acm.service.SettingJournalEnteriesService;
import com.acm.utils.dtos.SettingJournalEnteriesDTO;
import com.acm.utils.models.SettingJournalEnteries;
import com.acm.utils.models.SettingJournalEntryType;
import com.acm.utils.models.SettingMotifRejets;

/**
 * {@link SettingJournalEnteriesServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 0.2.0
 */
@Service
public class SettingJournalEnteriesServiceImpl implements SettingJournalEnteriesService {

	/** The Constant logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(SettingJournalEnteriesServiceImpl.class);

	/** The settingMotifRejets repository. */
	@Autowired
	private SettingJournalEnteriesRepository settingJournalEnteriesRepository;

	@Autowired
	private SettingJournalEntryTypeRepository settingJournalEntryTypeRepository;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The user Client client. */
	@Autowired
	private UserClient userClient;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingMotifRejetsService#save(java.lang.Long,
	 * com.acm.utils.dtos.SettingMotifRejetsDTO)
	 */
	@Override
	public SettingJournalEnteriesDTO save(SettingJournalEnteriesDTO settingJournalEnteriesDTO) {

		SettingJournalEnteries settingJournalEnteries =
				mapper.map(settingJournalEnteriesDTO, SettingJournalEnteries.class);
		CommonFunctions.mapperToSave(settingJournalEnteries, userClient, logger);

		return mapper.map(settingJournalEnteriesRepository.save(settingJournalEnteries),
				SettingJournalEnteriesDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingMotifRejetsService#find()
	 */
	@Override
	public List<SettingJournalEnteriesDTO> find() {

		List<SettingJournalEnteries> settingJournalEnteries =
				settingJournalEnteriesRepository.findAll();
		List<SettingJournalEnteriesDTO> settingJournalEnteriesDTOs = new ArrayList<>();
		settingJournalEnteries.forEach(settingJournalEnterie -> settingJournalEnteriesDTOs
				.add(mapper.map(settingJournalEnterie, SettingJournalEnteriesDTO.class)));
		return settingJournalEnteriesDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingMotifRejetsService#delete(java.lang.Long)
	 */
	@Override
	public void delete(Long id) throws ResourcesNotFoundException {

		// delete old Motif Rejet type
		settingJournalEnteriesRepository.deleteById(id);
		logger.info(CommonLoggerMessage.SUCCESFULL_DELETE,
				SettingMotifRejets.class.getSimpleName());

	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingJournalEnteriesService#saveAll(java.util.List)
	 */
	@Override
	public List<SettingJournalEnteriesDTO> saveAll(
			List<SettingJournalEnteriesDTO> settingJournalEnteriesDTOs, Long settingJournalTypeId) {

		SettingJournalEntryType settingJournalEntryType =
				settingJournalEntryTypeRepository.findById(settingJournalTypeId).get();
		settingJournalEnteriesRepository.deleteBySettingJournalEntryType(settingJournalEntryType);
		List<SettingJournalEnteries> listSettingJournalEnteries =
				new ArrayList<SettingJournalEnteries>();
		settingJournalEnteriesDTOs.forEach((item) -> {
			SettingJournalEnteries settingJournalEnteries =
					mapper.map(item, SettingJournalEnteries.class);
			settingJournalEnteries.setSettingJournalEntryType(settingJournalEntryType);
			CommonFunctions.mapperToSave(settingJournalEnteries, userClient, logger);
			listSettingJournalEnteries.add(settingJournalEnteries);

		});
		List<SettingJournalEnteries> newSettingJournalEnteries =
				settingJournalEnteriesRepository.saveAll(listSettingJournalEnteries);
		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE,
				SettingJournalEnteries.class.getSimpleName());

		return newSettingJournalEnteries.stream()
				.map(source -> mapper.map(source, SettingJournalEnteriesDTO.class))
				.collect(Collectors.toList());
	}

}
