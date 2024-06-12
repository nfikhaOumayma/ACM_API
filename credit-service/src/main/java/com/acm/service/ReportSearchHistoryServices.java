/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.ReportSearchHistoryDTO;

/**
 * {@link ReportSearchHistoryServices} class.
 *
 * @author HaythemBenizid
 * @since 1.0.2
 */
public interface ReportSearchHistoryServices {

	/**
	 * Save new entry.
	 * 
	 * @author HaythemBenizid
	 * @param reportSearchHistoryDTO the reportSearchHistory DTO
	 * @return the reportSearchHistory DTO
	 */
	ReportSearchHistoryDTO save(ReportSearchHistoryDTO reportSearchHistoryDTO);

	/**
	 * The method used for updating the given {@link ReportSearchHistoryDTO} by ID.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @param reportSearchHistoryDTO the reportSearchHistory DTO
	 * @return the ReportSearchHistoryService DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	ReportSearchHistoryDTO save(Long id, ReportSearchHistoryDTO reportSearchHistoryDTO)
			throws ResourcesNotFoundException;

	/**
	 * Find by given params for connected user.
	 *
	 * @author HaythemBenizid
	 * @param reportSearchHistoryDTO the report search history DTO
	 * @return the list
	 */
	List<ReportSearchHistoryDTO> find(ReportSearchHistoryDTO reportSearchHistoryDTO);

	/**
	 * Delete {@link reportSearchHistoryDTO} by ID.
	 *
	 * @author HaythemBenizid
	 * @param reportSearchHistoryDTO the report search history DTO
	 */
	void delete(ReportSearchHistoryDTO reportSearchHistoryDTO);
}
