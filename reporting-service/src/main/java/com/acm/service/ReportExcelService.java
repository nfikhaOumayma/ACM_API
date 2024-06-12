/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */

package com.acm.service;

import java.io.IOException;
import java.util.Map;

import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.LoanScheduleDTO;
import com.acm.utils.dtos.ReportingDTO;
import com.acm.utils.dtos.ThirdPartyHistoriqueDTO;

/**
 * {@link ReportExcelService} class.
 *
 * @author HaythemBenizid
 * @since 1.1.1
 */
public interface ReportExcelService {

	/**
	 * Generate excel loan application.
	 *
	 * @author HaythemBenizid
	 * @param reportingDTO the reporting DTO
	 * @return the byte[]
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	byte[] generateExcelLoanApplication(ReportingDTO reportingDTO) throws IOException;

	/**
	 * Generate excel collection followup report.
	 *
	 * @author HaythemBenizid
	 * @param reportingDTO the reporting DTO
	 * @return the byte[]
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	byte[] generateExcelCollectionFollowupReport(ReportingDTO reportingDTO) throws IOException;

	/**
	 * Generate AML report.
	 *
	 * @author Yesser Somai
	 * @param thirdPartyHistoriqueDTO the third party historique DTO
	 * @return the byte[]
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	byte[] generateAMLReport(ThirdPartyHistoriqueDTO thirdPartyHistoriqueDTO) throws IOException;

	/**
	 * /** Generate excel schedule.
	 *
	 * @author Ines Dridi
	 * @param loanSchedule the loan schedule
	 * @return the byte[]
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	byte[] generateExcelSchedule(LoanScheduleDTO loanSchedule) throws IOException;

	/**
	 * Calculate financial report.
	 *
	 * @param loanDTO the loan DTO
	 * @return the map
	 */
	Map<String, Object> calculateFinancialReport(LoanDTO loanDTO);

}
