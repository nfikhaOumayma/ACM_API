package com.acm.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.exceptions.type.WorkFlowSettingException;
import com.acm.service.ItemRiskSettingService;
import com.acm.utils.dtos.ItemDTO;
import com.acm.utils.dtos.ItemRiskSettingDTO;

/**
 * The Class ItemRiskSettingController.
 */
@RestController
@RequestMapping("/item-risk")
public class ItemRiskSettingController {

	/** The item risk setting service. */
	@Autowired
	ItemRiskSettingService itemRiskSettingService;

	/**
	 * Find.
	 *
	 * @param itemDTO the item DTO
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws WorkFlowSettingException the work flow setting exception
	 */
	@PostMapping("/")
	public List<ItemRiskSettingDTO> find(@RequestBody ItemDTO itemDTO)
			throws ResourcesNotFoundException, WorkFlowSettingException {

		return itemRiskSettingService.findByItem(itemDTO);
	}

	/**
	 * Save.
	 *
	 * @param itemInstanceRiskSettingDTO the item instance risk setting DTO
	 * @return the list
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/save")
	public List<ItemRiskSettingDTO> save(
			@RequestBody List<ItemRiskSettingDTO> itemInstanceRiskSettingDTO)
			throws ResourcesNotFoundException {

		return itemRiskSettingService.save(itemInstanceRiskSettingDTO);
	}

}
