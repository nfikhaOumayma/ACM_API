/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

import com.acm.utils.dtos.ReportingDTO;
import com.acm.utils.dtos.ReportingSchedulesStatusDTO;

/**
 * {@link LoanReportingService} interface.
 *
 * @author HaythemBenizid
 * @since 1.1.2
 */
public interface LoanReportingService {

	/**
	 * Find {@link List} of {@link ReportingSchedulesStatusDTO} data from ABACUS DB.
	 *
	 * @author HaythemBenizid
	 * @param reportingDTO the reporting DTO
	 * @return the list
	 */
	List<ReportingSchedulesStatusDTO> findSchedulesStatus(ReportingDTO reportingDTO);

}
