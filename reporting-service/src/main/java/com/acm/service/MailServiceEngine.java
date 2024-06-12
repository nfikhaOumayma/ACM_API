/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.lang.reflect.InvocationTargetException;

import com.acm.utils.dtos.CalendarEventDTO;
import com.acm.utils.dtos.CustomerContactDTO;
import com.acm.utils.dtos.MailCustomerDTO;
import com.acm.utils.dtos.MailDTO;
import com.acm.utils.dtos.MailIBLoanDTO;
import com.acm.utils.dtos.MailLoanDTO;
import com.acm.utils.enums.MailBuilderMethod;

/**
 * {@link MailServiceEngine} interface Service for sending emails..
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
public interface MailServiceEngine {

	/**
	 * Prepare and send mail Test.
	 *
	 * @param mailDTO the mail DTO
	 */
	void prepareAndSend(MailDTO mailDTO);

	/**
	 * Prepare and send Email by given params && {@link MailBuilderMethod} &&
	 * {@link MailNotificationType}.
	 *
	 * @author AbdelkarimTurki
	 * @author HaythemBenizid
	 * @param mailLoanDTO the loanMail DTO
	 * @param mailBuilderMethod the mail builder method
	 * @throws IllegalAccessException the illegal access exception
	 * @throws InvocationTargetException the invocation target exception
	 * @throws NoSuchMethodException the no such method exception
	 */
	void prepareAndSend(MailLoanDTO mailLoanDTO, MailBuilderMethod mailBuilderMethod)
			throws IllegalAccessException, InvocationTargetException, NoSuchMethodException;

	/**
	 * Prepare and send Email by given params && {@link MailBuilderMethod} &&
	 * {@link MailNotificationType}.
	 *
	 * @author Salmen Fatnassi
	 * @param mailIBLoanDTO the loanMail DTO
	 * @param mailBuilderMethod the mail builder method
	 */
	void prepareAndSend(MailIBLoanDTO mailIBLoanDTO, MailBuilderMethod mailBuilderMethod);

	/**
	 * Prepare and send Email by given params to customer && {@link MailBuilderMethod} &&
	 * {@link MailNotificationType}.
	 *
	 * @author HaythemBenizid
	 * @param mailCustomerDTO the mail customer DTO
	 * @param mailBuilderMethod the mail builder method
	 * @throws IllegalAccessException the illegal access exception
	 * @throws InvocationTargetException the invocation target exception
	 * @throws NoSuchMethodException the no such method exception
	 */
	void prepareAndSend(MailCustomerDTO mailCustomerDTO, MailBuilderMethod mailBuilderMethod)
			throws IllegalAccessException, InvocationTargetException, NoSuchMethodException;

	/**
	 * Prepare and send contact.
	 *
	 * @author Salmen Fatnassi
	 * @author HaythemBenizid
	 * @param customerContactDTO the customer contact DTO
	 */
	void prepareAndSendContact(CustomerContactDTO customerContactDTO);

	/**
	 * Prepare and send.
	 *
	 * @param calendarEventDTO the calendar event DTO
	 * @throws Exception the exception
	 */
	void prepareAndSend(CalendarEventDTO calendarEventDTO) throws Exception;

}
