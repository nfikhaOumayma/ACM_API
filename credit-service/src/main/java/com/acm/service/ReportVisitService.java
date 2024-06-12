/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.utils.dtos.ReportVisitDTO;

/**
 * {@link ReportVisitService} interface.
 *
 * @author YesserSomai
 * @since 0.2.0
 */
public interface ReportVisitService {

	/**
	 * Find {@link ReportVisitDTO} by given ID.
	 *
	 * @author YesserSomai
	 * @param id the id
	 * @return the ReportVisitService DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	ReportVisitDTO find(Long id) throws ResourcesNotFoundException;

	/**
	 * Find {@link List} of {@link ReportVisitDTO} by given params.
	 *
	 * @author YesserSomai
	 * @param acmReportVisitDTO the acm report visit DTO
	 * @return the list
	 */
	List<ReportVisitDTO> find(ReportVisitDTO acmReportVisitDTO);

	/**
	 * The method used for saving the given {@link ReportVisitDTO}.
	 *
	 * @author YesserSomai
	 * @param acmReportVisitDTO the acm report visit DTO
	 * @return the ReportVisitService DTO
	 */
	ReportVisitDTO save(ReportVisitDTO acmReportVisitDTO);

	/**
	 * The method used for updating the given {@link ReportVisitDTO} by ID.
	 *
	 * @author YesserSomai
	 * @param id the id
	 * @param acmReportVisitDTO the acm report visit DTO
	 * @return the ReportVisitService DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	ReportVisitDTO save(Long id, ReportVisitDTO acmReportVisitDTO)
			throws ResourcesNotFoundException;

	/**
	 * Delete {@link ReportVisitDTO} by given params.
	 *
	 * @author YesserSomai
	 * @param acmReportVisitDTO the acm report visit DTO
	 */
	void delete(ReportVisitDTO acmReportVisitDTO);
}
