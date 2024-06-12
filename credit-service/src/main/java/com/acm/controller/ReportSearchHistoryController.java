/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
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
import com.acm.service.ReportSearchHistoryServices;
import com.acm.utils.dtos.ReportSearchHistoryDTO;

/**
 * {@link ReportSearchHistoryController} class.
 *
 * @author HaythemBenizid
 * @since 1.0.2
 */
@RestController
@RequestMapping("/report-search-history")
public class ReportSearchHistoryController {

	/** The reportSearchHistory services. */
	@Autowired
	private ReportSearchHistoryServices reportSearchHistoryServices;

	/**
	 * Create ReportSearchHistory.
	 * 
	 * @author HaythemBenizid
	 * @param reportSearchHistoryDTO the reportSearchHistory DTO
	 * @return the reportSearchHistory DTO
	 */
	@PostMapping("/create")
	public ReportSearchHistoryDTO create(
			@RequestBody ReportSearchHistoryDTO reportSearchHistoryDTO) {

		return reportSearchHistoryServices.save(reportSearchHistoryDTO);
	}

	/**
	 * Update ReportSearchHistory.
	 *
	 * @author HaythemBenizid
	 * @param reportSearchHistoryDTO the reportSearchHistory DTO
	 * @return the reportSearchHistory DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update")
	public ReportSearchHistoryDTO update(@RequestBody ReportSearchHistoryDTO reportSearchHistoryDTO)
			throws ResourcesNotFoundException {

		return reportSearchHistoryServices.save(reportSearchHistoryDTO.getId(),
				reportSearchHistoryDTO);
	}

	/**
	 * Find by given params for connected user.
	 * 
	 * @author HaythemBenizid
	 * @param reportSearchHistoryDTO the report search history DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<ReportSearchHistoryDTO> find(
			@RequestBody ReportSearchHistoryDTO reportSearchHistoryDTO) {

		return reportSearchHistoryServices.find(reportSearchHistoryDTO);
	}

	/**
	 * Delete using ID.
	 * 
	 * @author HaythemBenizid
	 * @param id the id
	 */
	@DeleteMapping("/{id}")
	public void delete(@PathVariable("id") Long id) {

		reportSearchHistoryServices.delete(new ReportSearchHistoryDTO(id));
	}
}
