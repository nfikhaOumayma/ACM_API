package com.acm.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.ChargeFeesService;
import com.acm.utils.dtos.ChargeFeesDTO;

/**
 * The Class ChargeFeesController.
 */
@RestController
@RequestMapping("/charge-fees")
public class ChargeFeesController {

	/** The charge fees service. */
	@Autowired
	ChargeFeesService chargeFeesService;

	/**
	 * Find.
	 *
	 * @param chargeFeesDTO the charge fees DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<ChargeFeesDTO> find(@RequestBody ChargeFeesDTO chargeFeesDTO) {

		return chargeFeesService.find(chargeFeesDTO);
	}

	/**
	 * Creates the.
	 *
	 * @param chargeFeesDTO the charge fees DTO
	 * @return the charge fees DTO
	 */
	@PostMapping("/create")
	public ChargeFeesDTO create(@RequestBody ChargeFeesDTO chargeFeesDTO) {

		return chargeFeesService.save(chargeFeesDTO);
	}

	/**
	 * Creates the all.
	 *
	 * @param ChargeFeesDTOs the charge fees DT os
	 * @return the list
	 */
	@PostMapping("/create-all")
	public List<ChargeFeesDTO> createAll(@RequestBody List<ChargeFeesDTO> ChargeFeesDTOs) {

		return chargeFeesService.saveAll(ChargeFeesDTOs);
	}

	/**
	 * Update.
	 *
	 * @param chargeFeesDTO the charge fees DTO
	 * @return the charge fees DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update")
	public ChargeFeesDTO update(@RequestBody ChargeFeesDTO chargeFeesDTO)
			throws ResourcesNotFoundException {

		return chargeFeesService.save(chargeFeesDTO.getId(), chargeFeesDTO);
	}

	/**
	 * Delete.
	 *
	 * @param id the id
	 */
	@DeleteMapping("/delete/{id}")
	public void delete(@PathVariable("id") Long id) {

		chargeFeesService.delete(id);
	}
}
