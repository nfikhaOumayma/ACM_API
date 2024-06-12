/*
 * 
 */
package com.acm.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.SettingChargeFeeService;
import com.acm.utils.dtos.SettingChargeFeeDTO;

/**
 * The Class SettingChargeFeeController.
 */
@RestController
@RequestMapping("/setting-charge-fee")
public class SettingChargeFeeController {

	/** The setting charge fee service. */
	@Autowired
	private SettingChargeFeeService settingChargeFeeService;

	/**
	 * Find.
	 *
	 * @param settingChargeFeeDTO the setting charge fee DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<SettingChargeFeeDTO> find(@RequestBody SettingChargeFeeDTO settingChargeFeeDTO) {

		return settingChargeFeeService.find(settingChargeFeeDTO);
	}

	/**
	 * Create.
	 *
	 * @param settingChargeFeeDTO the setting charge fee DTO
	 * @return the setting charge fee DTO
	 */
	@PostMapping("/create")
	public SettingChargeFeeDTO create(@RequestBody SettingChargeFeeDTO settingChargeFeeDTO) {

		return settingChargeFeeService.save(settingChargeFeeDTO);
	}

	/**
	 * Update.
	 *
	 * @param settingChargeFeeDTO the setting charge fee DTO
	 * @return the setting charge fee DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update")
	public SettingChargeFeeDTO update(@RequestBody SettingChargeFeeDTO settingChargeFeeDTO)
			throws ResourcesNotFoundException {

		return settingChargeFeeService.save(settingChargeFeeDTO.getId(), settingChargeFeeDTO);
	}

	/**
	 * Find All.
	 *
	 * @return the list
	 */
	@GetMapping("/find-all")
	public List<SettingChargeFeeDTO> find() {

		return settingChargeFeeService.find();
	}

	/**
	 * Find by id.
	 *
	 * @param id the id
	 * @return the setting charge fee DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@GetMapping("/{id}")
	public SettingChargeFeeDTO findById(@PathVariable("id") Long id)
			throws ResourcesNotFoundException {

		return settingChargeFeeService.find(id);
	}
}
