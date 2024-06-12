/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.SettingDocumentTypeDTO;

/**
 * {@link SettingDocumentTypeService} interface.
 *
 * @author HaythemBenizid
 * @since 0.7.0
 */
public interface SettingDocumentTypeService {

	/**
	 * Find {@link List} of {@link SettingDocumentTypeDTO} by given params. using Querydsl.
	 *
	 * @author HaythemBenizid
	 * @param settingDocumentTypeDTO the setting document type DTO
	 * @return the list
	 */
	List<SettingDocumentTypeDTO> find(SettingDocumentTypeDTO settingDocumentTypeDTO);

	/**
	 * Find {@link SettingDocumentTypeDTO} by given ID.
	 *
	 * @author AbdelkarimTurki
	 * @param id the id
	 * @return the settingDocumentType DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	SettingDocumentTypeDTO find(Long id) throws ResourcesNotFoundException;

	/**
	 * Find all.
	 *
	 * @author AbdelkarimTurki
	 * @return the list
	 */
	List<SettingDocumentTypeDTO> find();

	/**
	 * update setting document type data in DB.
	 *
	 * @author AbdelkarimTurki
	 * @param id the id
	 * @param settingDocumentTypeDTO the setting document type DTO
	 * @return the settingDocumentType DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	SettingDocumentTypeDTO save(Long id, SettingDocumentTypeDTO settingDocumentTypeDTO)
			throws ResourcesNotFoundException;

	/**
	 * Save setting Document Type data in DB.
	 * 
	 * @author AbdelkarimTurki
	 * @param settingDocumentTypeDTO the setting document type DTO
	 * @return the settingDocumentType DTO
	 */
	SettingDocumentTypeDTO save(SettingDocumentTypeDTO settingDocumentTypeDTO);

	/**
	 * Update status document type.
	 *
	 * @param settingDocumentTypeDTO the setting document type DTO
	 * @param status the status
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	void updateStatus(SettingDocumentTypeDTO settingDocumentTypeDTO, Boolean status)
			throws ResourcesNotFoundException;

	/**
	 * Find doc type by step.
	 *
	 * @param idStep the id step
	 * @param idInstance the id instance
	 * @return the list
	 */
	List<SettingDocumentTypeDTO> findDocTypeByStep(Long idStep, Long idInstance);

	/**
	 * Find doc by category.
	 *
	 * @param category the category
	 * @param elementId the element id
	 * @return the setting document type DTO
	 */
	List<SettingDocumentTypeDTO> findDocByCategory(String category, Long elementId);

}
