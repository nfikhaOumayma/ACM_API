/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.exceptions.type.RenewalConditionSettingLoanException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.RenewalConditionService;
import com.acm.utils.dtos.CustomerAccountDTO;
import com.acm.utils.dtos.RenewalConditionDTO;
import com.acm.utils.dtos.RenewalConditionLoanDTO;

/**
 * {@link RenewalConditionController} class.
 *
 * @author idridi
 * @since 1.0.8
 */
@RestController
@RequestMapping("/renewal-condition")
public class RenewalConditionController {

	/** The renewal condition service. */
	@Autowired
	private RenewalConditionService renewalConditionService;

	/**
	 * Find.
	 * 
	 * @author idridi
	 * @param renewalConditionDTO the renewal condition DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<RenewalConditionDTO> find(@RequestBody RenewalConditionDTO renewalConditionDTO) {

		return renewalConditionService.find(renewalConditionDTO);
	}

	/**
	 * Creates the.
	 * 
	 * @author idridi
	 * @param renewalConditionDTO the renewal condition DTO
	 * @return the renewal condition DTO
	 */
	@PostMapping("/create")
	public RenewalConditionDTO create(@RequestBody RenewalConditionDTO renewalConditionDTO) {

		return renewalConditionService.save(renewalConditionDTO);
	}

	/**
	 * Update.
	 * 
	 * @author idridi
	 * @param renewalConditionDTO the renewal condition DTO
	 * @return the renewal condition DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update")
	public RenewalConditionDTO update(@RequestBody RenewalConditionDTO renewalConditionDTO)
			throws ResourcesNotFoundException {

		return renewalConditionService.save(renewalConditionDTO.getId(), renewalConditionDTO);
	}

	/**
	 * Delete.
	 * 
	 * @author idridi
	 * @param id the id
	 */
	@DeleteMapping("/delete/{id}")
	public void delete(@PathVariable("id") Long id) {

		renewalConditionService.delete(id);
	}

	/**
	 * Find by year and last paid amount.
	 *
	 * @author idridi
	 * @param renewalYear the renewal year
	 * @param lastPaidAmount the last paid amount
	 * @return the renewal condition DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@GetMapping("/find-renewal-condition-setting/{renewalYear}/{lastPaidAmount}")
	public RenewalConditionDTO findByYearAndLastPaidAmount(
			@PathVariable("renewalYear") Integer renewalYear,
			@PathVariable("lastPaidAmount") Long lastPaidAmount) throws ResourcesNotFoundException {

		return renewalConditionService.findByYearAndLastPaidAmount(renewalYear, lastPaidAmount);

	}

	/**
	 * Find renewal condition setting.
	 *
	 * @author idridi
	 * @param customerAccountDTO the customer account DTO
	 * @return the renewal condition loan DTO
	 * @throws RenewalConditionSettingLoanException the renewal condition setting loan exception
	 */
	@PostMapping("/get-setting-renewal-condition")
	public RenewalConditionLoanDTO findRenewalConditionSetting(
			@RequestBody CustomerAccountDTO customerAccountDTO)
			throws RenewalConditionSettingLoanException {

		return renewalConditionService.findRenewalConditionSetting(customerAccountDTO);
	}

}
