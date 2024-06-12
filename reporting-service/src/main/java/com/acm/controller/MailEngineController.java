/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.lang.reflect.InvocationTargetException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.service.MailServiceEngine;
import com.acm.utils.dtos.CalendarEventDTO;
import com.acm.utils.dtos.CustomerContactDTO;
import com.acm.utils.dtos.MailCustomerDTO;
import com.acm.utils.dtos.MailDTO;
import com.acm.utils.dtos.MailIBLoanDTO;
import com.acm.utils.dtos.MailLoanDTO;

/**
 * the {@link MailEngineController} class Method: sending mails.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
@RestController
@RequestMapping("/mailsender")
public class MailEngineController {

	/** The mail service engine. */
	@Autowired
	private MailServiceEngine mailServiceEngine;

	/**
	 * Send.
	 *
	 * @param mailDTO the mail DTO
	 * @return the mail DTO
	 */
	@PostMapping("/send")
	public MailDTO send(@RequestBody MailDTO mailDTO) {

		mailServiceEngine.prepareAndSend(mailDTO);
		return mailDTO;
	}

	/**
	 * Send the mail.
	 *
	 * @author AbdelkarimTurki
	 * @author HaythemBenizid
	 * @param mailLoanDTO the loanMail DTO
	 * @return the mail DTO
	 * @throws IllegalAccessException the illegal access exception
	 * @throws InvocationTargetException the invocation target exception
	 * @throws NoSuchMethodException the no such method exception
	 */
	@PostMapping("/send-email")
	public MailLoanDTO sendEmail(@RequestBody MailLoanDTO mailLoanDTO)
			throws IllegalAccessException, InvocationTargetException, NoSuchMethodException {

		mailServiceEngine.prepareAndSend(mailLoanDTO, mailLoanDTO.getMailBuilderMethod());
		return mailLoanDTO;
	}

	/**
	 * Send the mail to customer.
	 *
	 * @author HaythemBenizid
	 * @param mailCustomerDTO the mail customer DTO
	 * @return the mail DTO
	 * @throws IllegalAccessException the illegal access exception
	 * @throws InvocationTargetException the invocation target exception
	 * @throws NoSuchMethodException the no such method exception
	 */
	@PostMapping("/send-email-customer")
	public MailCustomerDTO sendEmail(@RequestBody MailCustomerDTO mailCustomerDTO)
			throws IllegalAccessException, InvocationTargetException, NoSuchMethodException {

		mailServiceEngine.prepareAndSend(mailCustomerDTO, mailCustomerDTO.getMailBuilderMethod());
		return mailCustomerDTO;
	}

	/**
	 * Send the mail to customer.
	 *
	 * @author Salmen Fatnassi
	 * @param mailIBLoanDTO the mail loanib DTO
	 * @return the mail DTO
	 */
	@PostMapping("/send-email-ibloan")
	public MailIBLoanDTO sendEmail(@RequestBody MailIBLoanDTO mailIBLoanDTO) {

		mailServiceEngine.prepareAndSend(mailIBLoanDTO, mailIBLoanDTO.getMailBuilderMethod());
		return mailIBLoanDTO;
	}

	/**
	 * Send the contact mail.
	 *
	 * @author Salmen Fatnassi
	 * @param customerContactDTO the customer contact DTO
	 * @return the mail DTO
	 */
	@PostMapping("/send-contact-email")
	public CustomerContactDTO sendEmail(@RequestBody CustomerContactDTO customerContactDTO) {

		mailServiceEngine.prepareAndSendContact(customerContactDTO);
		return customerContactDTO;
	}

	/**
	 * Send meeting invitation.
	 *
	 * @param calendarEventDTO the calendar event DTO
	 * @throws Exception the exception
	 */
	@PostMapping("/meetingInvitation")
	public void sendMeetingInvitation(@RequestBody CalendarEventDTO calendarEventDTO)
			throws Exception {

		mailServiceEngine.prepareAndSend(calendarEventDTO);

	}
}
