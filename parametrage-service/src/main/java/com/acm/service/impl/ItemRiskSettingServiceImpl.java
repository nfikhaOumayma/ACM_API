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
import com.acm.exceptions.type.WorkFlowSettingException;
import com.acm.repository.ItemRiskSettingRepository;
import com.acm.service.ItemRiskSettingService;
import com.acm.service.SettingWorkFlowService;
import com.acm.utils.dtos.ItemDTO;
import com.acm.utils.dtos.ItemRiskSettingDTO;
import com.acm.utils.dtos.RiskSettingStepDTO;
import com.acm.utils.dtos.WorkFlowStepDTO;
import com.acm.utils.models.Item;
import com.acm.utils.models.ItemRiskSetting;
import com.acm.utils.validation.ACMValidationUtils;

/**
 * The Class ItemRiskSettingServiceImpl.
 */
@Service
public class ItemRiskSettingServiceImpl implements ItemRiskSettingService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(ItemRiskSettingServiceImpl.class);

	/** The collection note repository. */

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;
	/** The user Client client. */
	@Autowired
	private UserClient userClient;

	/** The item risk setting repository. */
	@Autowired
	ItemRiskSettingRepository itemRiskSettingRepository;

	/** The setting work flow service. */
	@Autowired
	SettingWorkFlowService settingWorkFlowService;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ItemRiskSettingService#findByItem(com.acm.utils.dtos.ItemDTO)
	 */
	@Override
	public List<ItemRiskSettingDTO> findByItem(ItemDTO itemDTO) throws WorkFlowSettingException {

		Item item = new Item();
		item.setId(itemDTO.getId());
		List<ItemRiskSetting> lstItemRisk = itemRiskSettingRepository.findByItem(item);
		WorkFlowStepDTO workFlowStepDTO = new WorkFlowStepDTO();
		workFlowStepDTO.setIdWorkFlowStep(itemDTO.getActualStep());
		List<WorkFlowStepDTO> workFlowStepDTOs = settingWorkFlowService.findSteps(workFlowStepDTO);
		List<RiskSettingStepDTO> riskSettingStepDTOs = workFlowStepDTOs.get(0).getListRiskSetting();
		List<ItemRiskSettingDTO> lstRiskSync = new ArrayList<>();
		if (!ACMValidationUtils.isNullOrEmpty(riskSettingStepDTOs)) {
			riskSettingStepDTOs.stream().forEach(riskSetting -> {

				List<ItemRiskSetting> matchingItems = lstItemRisk.stream()
						.filter(element -> element.getSettingTypeRisk().getId()
								.equals(riskSetting.getIdRiskSetting()))
						.collect(Collectors.toList());

				if (!matchingItems.isEmpty()) {
					// Get the first matching item
					ItemRiskSetting matchedItem = matchingItems.get(0);
					ItemRiskSettingDTO matchedItemDTO =
							mapper.map(matchedItem, ItemRiskSettingDTO.class);
					// Set the editable attribute in lstRiskSync
					matchedItemDTO.setEditable(riskSetting.getIsEditable());
					// Add the matched item to lstRiskSync
					lstRiskSync.add(matchedItemDTO);
				}

			});

		}
		return lstRiskSync;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ItemRiskSettingService#save(java.util.List)
	 */
	@Override
	public List<ItemRiskSettingDTO> save(List<ItemRiskSettingDTO> itemRiskSettingDTOs) {

		List<ItemRiskSetting> itemRiskSettings = new ArrayList<>();
		List<ItemRiskSettingDTO> itemRiskSettingDTOSaving = new ArrayList<>();
		itemRiskSettingDTOs.forEach(element -> {
			itemRiskSettings.add(mapper.map(element, ItemRiskSetting.class));
		});
		itemRiskSettingRepository.saveAll(itemRiskSettings).forEach(element -> {
			itemRiskSettingDTOSaving.add(mapper.map(element, ItemRiskSettingDTO.class));
		});
		return itemRiskSettingDTOSaving;
	}

}
