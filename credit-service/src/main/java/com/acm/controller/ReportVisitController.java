/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
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
import com.acm.service.ReportVisitService;
import com.acm.utils.dtos.ReportVisitDTO;

/**
 * This class @{link ReportVisitController} used to control all the ReportVisit requests.
 *
 * @author YesserSomai
 * @since 0.2.0
 */
@RestController
@RequestMapping("/report-visits")
public class ReportVisitController {

	/** The ReportVisit service. */
	@Autowired
	private ReportVisitService reportVisitService;

	/**
	 * Find ReportVisitF by id.
	 *
	 * @author YesserSomai
	 * @param id the id
	 * @return the report visit DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@GetMapping("/{id}")
	public ReportVisitDTO findById(@PathVariable("id") Long id) throws ResourcesNotFoundException {

		return reportVisitService.find(id);
	}

	/**
	 * Find {@link List} of {@link ReportVisitDTO} by Requested params.
	 *
	 * @author YesserSomai
	 * @param acmReportVisitDTO the acm report visit DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<ReportVisitDTO> find(@RequestBody ReportVisitDTO acmReportVisitDTO) {

		return reportVisitService.find(acmReportVisitDTO);
	}

	/**
	 * Create the ReportVisit.
	 *
	 * @author YesserSomai
	 * @param acmReportVisitDTO the acm report visit DTO
	 * @return the ReportVisit DTO
	 */
	@PostMapping("/create")
	public ReportVisitDTO create(@RequestBody ReportVisitDTO acmReportVisitDTO) {

		return reportVisitService.save(acmReportVisitDTO);
	}

	/**
	 * Update the ReportVisit by id.
	 * 
	 * @author YesserSomai
	 * @param acmReportVisitDTO the acmReportVisit DTO
	 * @return the acmReportVisit DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PutMapping("/update")
	public ReportVisitDTO update(@RequestBody ReportVisitDTO acmReportVisitDTO)
			throws ResourcesNotFoundException {

		return reportVisitService.save(acmReportVisitDTO.getIdReportVisit(), acmReportVisitDTO);
	}

}
