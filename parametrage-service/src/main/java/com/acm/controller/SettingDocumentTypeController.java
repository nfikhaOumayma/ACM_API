/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.util.ArrayList;
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
import com.acm.service.SettingDocumentTypeService;
import com.acm.utils.dtos.AcmStatutsDTO;
import com.acm.utils.dtos.SettingDocumentTypeDTO;
import com.acm.utils.enums.DocumentTypeCatgory;

/**
 * The Class SettingDocumentTypeController.
 */
@RestController
@RequestMapping("/setting-document-types")
public class SettingDocumentTypeController {

	/** The SettingDocumentType service. */
	@Autowired
	private SettingDocumentTypeService settingDocumentTypeService;

	/**
	 * Find list SettingDocumentType by given params.
	 *
	 * @param settingDocumentTypeDTO the setting document type DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<SettingDocumentTypeDTO> find(
			@RequestBody SettingDocumentTypeDTO settingDocumentTypeDTO) {

		return settingDocumentTypeService.find(settingDocumentTypeDTO);
	}

	/**
	 * Creates the SettingDocumentTypeDTO by new value.
	 * 
	 * @author AbdelkarimTurki
	 * @param settingDocumentTypeDTO the settingDocumentType DTO
	 * @return the settingDocumentType DTO
	 */
	@PostMapping("/create")
	public SettingDocumentTypeDTO create(
			@RequestBody SettingDocumentTypeDTO settingDocumentTypeDTO) {

		return settingDocumentTypeService.save(settingDocumentTypeDTO);
	}

	/**
	 * Update the parameter by id.
	 * 
	 * @author AbdelkarimTurki
	 * @param settingDocumentTypeDTO the settingDocumentType DTO
	 * @return the settingDocumentType DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update")
	public SettingDocumentTypeDTO update(@RequestBody SettingDocumentTypeDTO settingDocumentTypeDTO)
			throws ResourcesNotFoundException {

		return settingDocumentTypeService.save(settingDocumentTypeDTO.getId(),
				settingDocumentTypeDTO);
	}

	/**
	 * Load category type.
	 * 
	 * @author AbdelkarimTurki
	 * @return the list
	 */
	@GetMapping("/document-type-category")
	public List<AcmStatutsDTO> loadCategoryType() {

		List<AcmStatutsDTO> acmStatutsDTOs = new ArrayList<>();
		acmStatutsDTOs.add(new AcmStatutsDTO(DocumentTypeCatgory.LOAN.categoryId(),
				DocumentTypeCatgory.LOAN.name()));
		acmStatutsDTOs.add(new AcmStatutsDTO(DocumentTypeCatgory.CLIENT.categoryId(),
				DocumentTypeCatgory.CLIENT.name()));
		acmStatutsDTOs.add(new AcmStatutsDTO(DocumentTypeCatgory.ASSIGN_DOCUMENT.categoryId(),
				DocumentTypeCatgory.ASSIGN_DOCUMENT.name()));
		acmStatutsDTOs.add(new AcmStatutsDTO(DocumentTypeCatgory.COLLECTION.categoryId(),
				DocumentTypeCatgory.COLLECTION.name()));
		acmStatutsDTOs.add(new AcmStatutsDTO(DocumentTypeCatgory.LEGAL.categoryId(),
				DocumentTypeCatgory.LEGAL.name()));
		acmStatutsDTOs.add(new AcmStatutsDTO(DocumentTypeCatgory.GENERIC_WOKFLOW.categoryId(),
				DocumentTypeCatgory.GENERIC_WOKFLOW.name()));
		acmStatutsDTOs.add(new AcmStatutsDTO(DocumentTypeCatgory.SUPPLIER.categoryId(),
				DocumentTypeCatgory.SUPPLIER.name()));
		acmStatutsDTOs.add(new AcmStatutsDTO(DocumentTypeCatgory.CONVENTION.categoryId(),
				DocumentTypeCatgory.CONVENTION.name()));
		acmStatutsDTOs.add(new AcmStatutsDTO(DocumentTypeCatgory.THIRD_PARTY.categoryId(),
				DocumentTypeCatgory.THIRD_PARTY.name()));
		return acmStatutsDTOs;
	}

	/**
	 * disable document type from database.
	 *
	 * @author AbdelkarimTurki
	 * @param settingDocumentTypeDTO the setting Document Type DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/disable-document-type")
	public void disableDocumentType(@RequestBody SettingDocumentTypeDTO settingDocumentTypeDTO)
			throws ResourcesNotFoundException {

		settingDocumentTypeService.updateStatus(settingDocumentTypeDTO, Boolean.FALSE);
	}

	/**
	 * enable document type from database.
	 *
	 * @author AbdelkarimTurki
	 * @param settingDocumentTypeDTO the setting Document Type DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/enable-document-type")
	public void enableDocumentType(@RequestBody SettingDocumentTypeDTO settingDocumentTypeDTO)
			throws ResourcesNotFoundException {

		settingDocumentTypeService.updateStatus(settingDocumentTypeDTO, Boolean.TRUE);
	}

	/**
	 * Find all category type.
	 * 
	 * @author AbdelkarimTurki
	 * @return the list
	 */
	@GetMapping("/find-all")
	public List<SettingDocumentTypeDTO> find() {

		return settingDocumentTypeService.find();
	}

	/**
	 * Find doc type by step.
	 *
	 * @param idStep the id step
	 * @param idInstance the id instance
	 * @return the list
	 */
	@GetMapping("/step/{idStep}/{idInstance}")
	public List<SettingDocumentTypeDTO> findDocTypeByStep(@PathVariable("idStep") Long idStep,
			@PathVariable("idInstance") Long idInstance) {

		return settingDocumentTypeService.findDocTypeByStep(idStep, idInstance);
	}

	/**
	 * Find doc type by step.
	 *
	 * @param category the category
	 * @param elementId the element id
	 * @return the setting document type DTO
	 */
	@GetMapping("/category/{category}/{elementId}")
	public List<SettingDocumentTypeDTO> findDocTypeByStep(@PathVariable("category") String category,
			@PathVariable("elementId") Long elementId) {

		return settingDocumentTypeService.findDocByCategory(category, elementId);
	}

}
