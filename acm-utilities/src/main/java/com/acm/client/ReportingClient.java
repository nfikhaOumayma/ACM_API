/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.client;

import org.springframework.cloud.netflix.ribbon.RibbonClient;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;

import com.acm.configuration.feignclient.ClientConfiguration;
import com.acm.configuration.feignclient.LoadbalancerRuleFeignConfiguration;
import com.acm.utils.dtos.CalendarEventDTO;
import com.acm.utils.dtos.CustomerContactDTO;
import com.acm.utils.dtos.MailCustomerDTO;
import com.acm.utils.dtos.MailDTO;
import com.acm.utils.dtos.MailIBLoanDTO;
import com.acm.utils.dtos.MailLoanDTO;
import com.acm.utils.dtos.ReportDTO;

/**
 * The {@link ReportingClient} Interface. to inject in order to consume services from
 * reporting-service
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
@FeignClient(value = "reporting-service", configuration = ClientConfiguration.class,
		decode404 = true)
@RibbonClient(name = "reporting-service", configuration = LoadbalancerRuleFeignConfiguration.class)
public interface ReportingClient {
	/**
	 * Generate Report Jasper LoanReportDTO.
	 * 
	 * @author MoezMhiri
	 * @param loanReportDTO the loan report DTO
	 * @return the byte[]
	 */
	@PostMapping("/generatePDF/loanReport")
	byte[] generatePDF(@RequestBody ReportDTO loanReportDTO);

	/**
	 * Send mail by given parameters.
	 * 
	 * @author HaythemBenizid
	 * @param mailDTO the mail DTO
	 */
	@RequestMapping("/mailsender/send")
	void sendMail(@RequestBody MailDTO mailDTO);

	/**
	 * Send mail by given parameters (USED ONLY by BATCH).
	 *
	 * @author HaythemBenizid
	 * @param mailDTO the mail DTO
	 * @param token the token
	 */
	@RequestMapping("/mailsender/send")
	void sendMail(@RequestBody MailDTO mailDTO, @RequestHeader("Authorization") String token);

	/**
	 * Send the mail by given parameters (LOAN).
	 * 
	 * @author AbdelkarimTurki
	 * @author HaythemBenizid
	 * @param mailLoanDTO the loanMail DTO
	 */
	@RequestMapping("/mailsender/send-email")
	void sendEmail(@RequestBody MailLoanDTO mailLoanDTO);

	/**
	 * Send the mail to customer by given parameters.
	 *
	 * @author HaythemBenizid
	 * @param mailCustomerDTO the mail customer DTO
	 */
	@RequestMapping("/mailsender/send-email-customer")
	void sendEmail(@RequestBody MailCustomerDTO mailCustomerDTO);

	/**
	 * Send mail.
	 * 
	 * @author Salmen Fatnassi
	 * @param mailIBLoanDTO the mail ibloan DTO
	 */
	@RequestMapping("/mailsender/send-email-ibloan")
	void sendEmail(@RequestBody MailIBLoanDTO mailIBLoanDTO);

	/**
	 * Send contact email.
	 *
	 * @author Salmen Fatnassi
	 * @param customerContactDTO the customer contact DTO
	 */
	@RequestMapping("/mailsender/send-contact-email")
	void sendContactEmail(@RequestBody CustomerContactDTO customerContactDTO);

	/**
	 * Send meting email.
	 *
	 * @param calendarEventDTO the calendar event DTO
	 */
	@PostMapping("/mailsender/meetingInvitation")
	void sendMetingEmail(@RequestBody CalendarEventDTO calendarEventDTO);

	/**
	 * Generate report.
	 *
	 * @param reportDTO the report DTO
	 * @return the byte[]
	 */
	@PostMapping("/edition/generate-report")
	byte[] generateReport(@RequestBody ReportDTO reportDTO);
}
