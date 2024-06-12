package com.acm.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.StepRiskSettingService;
import com.acm.utils.dtos.ItemDTO;
import com.acm.utils.dtos.StepRiskSettingDTO;

/**
 * The Class StepRiskSettingController.
 */
@RestController
@RequestMapping("/step-risk")
public class StepRiskSettingController {

	/** The step risk setting service. */
	@Autowired
	StepRiskSettingService stepRiskSettingService;

	/**
	 * Find.
	 *
	 * @param itemDTO the item DTO
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/")
	public List<StepRiskSettingDTO> find(@RequestBody ItemDTO itemDTO)
			throws ResourcesNotFoundException {

		return stepRiskSettingService.findByItemStep(itemDTO);
	}

	/**
	 * Find.
	 *
	 * @param stepRiskSettingDTOs the step risk setting DT os
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/save")
	public List<StepRiskSettingDTO> find(@RequestBody List<StepRiskSettingDTO> stepRiskSettingDTOs)
			throws ResourcesNotFoundException {

		return stepRiskSettingService.save(stepRiskSettingDTOs);
	}

}
