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
import com.acm.repository.StepRiskSettingRepository;
import com.acm.service.StepRiskSettingService;
import com.acm.utils.dtos.ItemDTO;
import com.acm.utils.dtos.StepRiskSettingDTO;
import com.acm.utils.models.StepRiskSetting;
import com.acm.utils.models.WorkFlowStep;

/**
 * The Class StepRiskSettingServiceImpl.
 */
@Service
public class StepRiskSettingServiceImpl implements StepRiskSettingService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(StepRiskSettingServiceImpl.class);

	/** The collection note repository. */

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;
	/** The user Client client. */
	@Autowired
	private UserClient userClient;

	/** The step risk setting repository. */
	@Autowired
	StepRiskSettingRepository stepRiskSettingRepository;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.StepRiskSettingService#findByItemStep(com.acm.utils.dtos.ItemDTO)
	 */
	@Override
	public List<StepRiskSettingDTO> findByItemStep(ItemDTO itemDTO) {

		WorkFlowStep workFlowStep = new WorkFlowStep();
		workFlowStep.setIdWorkFlowStep(itemDTO.getActualStep());
		List<StepRiskSetting> lstStepWfRisk =
				stepRiskSettingRepository.findByWorkFlowStep(workFlowStep);
		List<StepRiskSettingDTO> stepWfRiskDTOs = new ArrayList<>();
		lstStepWfRisk.forEach(element -> {
			stepWfRiskDTOs.add(mapper.map(element, StepRiskSettingDTO.class));
		});

		return stepWfRiskDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.StepRiskSettingService#save(java.util.List)
	 */
	@Override
	public List<StepRiskSettingDTO> save(List<StepRiskSettingDTO> itemWfRiskSettingDTOs) {

		List<StepRiskSetting> stepRiskSettings = new ArrayList<>();
		List<StepRiskSettingDTO> stepRiskSettingDTOSaving = new ArrayList<>();
		itemWfRiskSettingDTOs.forEach(element -> {
			stepRiskSettings.add(mapper.map(element, StepRiskSetting.class));
		});
		stepRiskSettingRepository.saveAll(stepRiskSettings).forEach(element -> {
			stepRiskSettingDTOSaving.add(mapper.map(element, StepRiskSettingDTO.class));
		});
		return stepRiskSettingDTOSaving;
	}

}
