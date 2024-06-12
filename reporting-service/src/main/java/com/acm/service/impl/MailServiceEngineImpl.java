/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeParseException;
import java.util.Date;
import java.util.Properties;

import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.Multipart;
import javax.mail.PasswordAuthentication;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeBodyPart;
import javax.mail.internet.MimeMessage;
import javax.mail.internet.MimeMultipart;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.mail.MailException;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.JavaMailSenderImpl;
import org.springframework.mail.javamail.MimeMessagePreparator;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import com.acm.constants.common.CommonConstants;
import com.acm.service.MailServiceEngine;
import com.acm.service.mailbuilder.MailContentBuilder;
import com.acm.utils.dtos.CalendarEventDTO;
import com.acm.utils.dtos.CalendarOutlookRequest;
import com.acm.utils.dtos.CustomerContactDTO;
import com.acm.utils.dtos.MailCustomerDTO;
import com.acm.utils.dtos.MailDTO;
import com.acm.utils.dtos.MailIBLoanDTO;
import com.acm.utils.dtos.MailLoanDTO;
import com.acm.utils.enums.MailBuilderMethod;
import com.acm.utils.validation.ACMValidationUtils;

/**
 * {@link MailServiceEngineImpl} class Service for sending emails.
 * <p>
 * We use the @Async annotation to send emails asynchronously.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
@Service
public class MailServiceEngineImpl implements MailServiceEngine {

	/** Default Mode is INFO. */
	private static final Logger logger = LoggerFactory.getLogger(MailServiceEngineImpl.class);

	/** The mail sender. */
	@Autowired
	private JavaMailSender mailSender;

	/** The mail content builder. */
	@Autowired
	private MailContentBuilder mailContentBuilder;

	/** The mail host. */
	@Value("${com.acm.mail.host}")
	private String mailHost;

	/** The mail port. */
	@Value("${com.acm.mail.port}")
	private String mailPort;

	/** The mail user name. */
	@Value("${com.acm.mail.username}")
	private String mailUserName;

	/** The mail PWD. */
	@Value("${com.acm.mail.password}")
	private String mailPWD;

	/** The mail SMTP auth. */
	@Value("${com.acm.mail.smtp.auth}")
	private String mailSMTPAuth;

	/** The mail smtp starttls enable. */
	@Value("${com.acm.mail.smtp.starttls.enable}")
	private String mailSmtpStarttlsEnable;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.MailServiceEngine#prepareAndSend(com.acm.utils.dtos.MailDTO)
	 */
	@Async
	@Override
	public void prepareAndSend(MailDTO mailDTO) {

		logger.debug("############ prepareAndSend : MailDTO ##############");
		// preparing mail content
		String content = mailContentBuilder.build(mailDTO);
		// call generic method to send mail
		try {
			send(content, mailDTO);
		}
		catch (MessagingException e) {
			logger.error("Failed to send Mail {}", e.getMessage());
			e.printStackTrace();
		}
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.MailServiceEngine#prepareAndSend(com.acm.utils.dtos.MailLoanDTO,
	 * com.acm.utils.enums.MailBuilderMethod )
	 */
	@Async
	@Override
	public void prepareAndSend(MailLoanDTO mailLoanDTO, MailBuilderMethod mailBuilderMethod)
			throws IllegalAccessException, InvocationTargetException, NoSuchMethodException {

		logger.debug("############ prepareAndSend : MailLoanDTO ##############");
		// preparing mail
		Method mailContentMethod = mailContentBuilder.getClass()
				.getMethod(mailBuilderMethod.methodName(), MailLoanDTO.class);
		String content = (String) mailContentMethod.invoke(mailContentBuilder, mailLoanDTO);
		try {
			send(content, mailLoanDTO.getMailDTO());
		}
		catch (MessagingException e) {
			logger.error("Failed to send Mail {}", e.getMessage());
			e.printStackTrace();
		}
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.MailServiceEngine#prepareAndSend(com.acm.utils.dtos.MailCustomerDTO,
	 * com.acm.utils.enums.MailBuilderMethod)
	 */
	@Async
	@Override
	public void prepareAndSend(MailCustomerDTO mailCustomerDTO, MailBuilderMethod mailBuilderMethod)
			throws IllegalAccessException, InvocationTargetException, NoSuchMethodException {

		logger.debug("############ prepareAndSend : MailCustomerDTO ##############");
		// preparing mail
		Method mailContentMethod = mailContentBuilder.getClass()
				.getMethod(mailBuilderMethod.methodName(), MailCustomerDTO.class);
		String content = (String) mailContentMethod.invoke(mailContentBuilder, mailCustomerDTO);
		try {
			send(content, mailCustomerDTO.getMailDTO());
		}
		catch (MessagingException e) {
			logger.error("Failed to send Mail {}", e.getMessage());
			e.printStackTrace();
		}
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.MailServiceEngine#prepareAndSend(com.acm.utils.dtos.MailCustomerDTO,
	 * com.acm.utils.enums.MailBuilderMethod)
	 */
	@Async
	@Override
	public void prepareAndSend(MailIBLoanDTO mailIBLoanDTO, MailBuilderMethod mailBuilderMethod) {

		// preparing mail
		String content =
				mailContentBuilder.buildLoanRejectedClientEmailForMailCustomer(mailIBLoanDTO);
		MailDTO mailDTO = new MailDTO(mailIBLoanDTO.getMailDTO().getFrom(),
				mailIBLoanDTO.getMailDTO().getTo(), mailIBLoanDTO.getMailDTO().getSubject(),
				mailIBLoanDTO.getMailDTO().getContent());
		// call generic method to send mail
		try {
			send(content, mailDTO);
		}
		catch (MessagingException e) {
			logger.error("Failed to send Mail {}", e.getMessage());
			e.printStackTrace();
		}
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.MailServiceEngine#prepareAndSendContact(com.acm.utils.dtos.MailDTO)
	 */
	@Async
	@Override
	public void prepareAndSendContact(CustomerContactDTO customerContactDTO) {

		// preparing mail
		String content = mailContentBuilder.buildContactEmail(customerContactDTO);
		MailDTO mailDTO = new MailDTO(customerContactDTO.getFrom(), customerContactDTO.getTo(),
				customerContactDTO.getSubject(), customerContactDTO.getContent());
		// call generic method to send mail
		try {
			send(content, mailDTO);
		}
		catch (MessagingException e) {
			logger.error("Failed to send Mail {}", e.getMessage());
			e.printStackTrace();
		}
	}

	/**
	 * Send mail (Generic method).
	 * 
	 * @author HaythemBenizid
	 * @param messagePreparator the message preparator
	 * @param mailDTO the mail DTO
	 */
	@SuppressWarnings("unused")
	private void sendMail(MimeMessagePreparator messagePreparator, MailDTO mailDTO) {

		try {
			mailSender.send(messagePreparator);
			logger.debug("Sent email to User '{}' :: DONE", mailDTO.getTo());
		}
		catch (MailException e) {
			// runtime exception
			logger.error("Failed to send Mail {}", e.getMessage());
			e.printStackTrace();
		}
	}

	/**
	 * Send mail (Generic method Using {@link javax.mail.Transport}).
	 * 
	 * @author HaythemBenizid
	 * @param content the content
	 * @param mailDTO the mail DTO
	 * @throws MessagingException the messaging exception
	 */
	private void send(String content, MailDTO mailDTO) throws MessagingException {

		logger.debug("Start sending mail");
		final String username = mailUserName;
		final String pwd = mailPWD;
		Properties props = new Properties();
		props.put("mail.smtp.auth", mailSMTPAuth);
		props.put("mail.smtp.starttls.enable", mailSmtpStarttlsEnable);
		props.put("mail.smtp.host", mailHost);
		props.put("mail.smtp.port", mailPort);
		props.put("mail.mime.charset", StandardCharsets.UTF_8.name());

		logger.debug(
				"Mail send from config : Username : {} | Password : {} | SMPT AUTH {} | starttls {} | host {} | port {}",
				mailUserName, mailPWD, mailSMTPAuth, mailSmtpStarttlsEnable, mailHost, mailPort);
		Session session = Session.getInstance(props, new javax.mail.Authenticator() {
			@Override
			protected PasswordAuthentication getPasswordAuthentication() {

				return new PasswordAuthentication(username, pwd);
			}
		});
		Message message = new MimeMessage(session);
		message.setFrom(new InternetAddress(mailDTO.getFrom(), false));
		message.setRecipients(Message.RecipientType.TO, InternetAddress.parse(mailDTO.getTo()));
		message.setSubject(CommonConstants.APP_CLIENT + " : " + mailDTO.getSubject());
		message.setSentDate(new Date());
		if (!ACMValidationUtils.isNullOrEmpty(mailDTO.getCc())) {
			message.setRecipients(Message.RecipientType.CC, InternetAddress.parse(mailDTO.getCc()));
		}
		MimeBodyPart mimeBodyPart = new MimeBodyPart();
		mimeBodyPart.setContent(content, "text/html; charset=utf-8");
		Multipart multipart = new MimeMultipart();
		multipart.addBodyPart(mimeBodyPart);

		message.setContent(multipart, "text/html; charset=utf-8");
		Transport.send(message);
		logger.debug("sending mail '{}' :: DONE", mailDTO.getTo());
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.MailServiceEngine#prepareAndSend(java.lang.String, java.lang.String,
	 * java.util.Date, java.util.Date, java.lang.String)
	 */
	@Override
	public void prepareAndSend(CalendarEventDTO calendarEventDTO) throws Exception {

		JavaMailSenderImpl mailSender = new JavaMailSenderImpl();
		mailSender.setUsername(mailUserName);
		mailSender.setPassword(mailPWD);
		Properties properties = new Properties();
		properties.put("mail.smtp.auth", mailSMTPAuth);
		properties.put("mail.smtp.starttls.enable", mailSmtpStarttlsEnable);
		properties.put("mail.smtp.host", mailHost);
		properties.put("mail.smtp.port", mailPort);
		properties.put("mail.mime.charset", StandardCharsets.UTF_8.name());
		String body = prepareBody(calendarEventDTO);
		String location = prepareLocation(calendarEventDTO);
		mailSender.setJavaMailProperties(properties);
		CalendarServiceImpl calendarService = new CalendarServiceImpl(mailSender);
		calendarService.sendCalendarInvite(mailUserName,
				new CalendarOutlookRequest.Builder().withSubject(calendarEventDTO.getLibelleEvent())
						.withLocation(location).withBody(body)
						.withToEmail(calendarEventDTO.getUserEmail())
						.withMeetingStartTime(formatterDate(calendarEventDTO.getDateDebut()))
						.withMeetingEndTime(formatterDate(calendarEventDTO.getDateFin())).build());

	}

	/**
	 * Prepare location.
	 *
	 * @author kouali
	 * @param calendarEventDTO the calendar event DTO
	 * @return the string
	 */
	private String prepareLocation(CalendarEventDTO calendarEventDTO) {

		String location = calendarEventDTO.getCustomerName();
		if (calendarEventDTO.getPlace() != null && !calendarEventDTO.getPlace().equals("")) {
			location = location + "-" + calendarEventDTO.getPlace();
		}

		return location;
	}

	/**
	 * Prepare body.
	 *
	 * @author kouali
	 * @param calendarEventDTO the calendar event DTO
	 * @return the string
	 */
	private String prepareBody(CalendarEventDTO calendarEventDTO) {

		String body = "Category : ";
		body = body + calendarEventDTO.getCategory() + "\\n" + "Customer name : "
				+ calendarEventDTO.getCustomerName() + "\\n";
		body = body + "Customer number : " + calendarEventDTO.getCustomerNumber();

		if (calendarEventDTO.getFullNameParticipants() != null
				&& !calendarEventDTO.getFullNameParticipants().equals("")) {
			body = body + "\\n" + "Participants : " + calendarEventDTO.getFullNameParticipants();
		}

		if (calendarEventDTO.getDescription() != null
				&& !calendarEventDTO.getDescription().equals("")) {
			body = body + "\\n" + "Description : "
					+ calendarEventDTO.getDescription().replace("\n", "\\n");
		}

		return body;
	}

	/**
	 * Formatter date.
	 *
	 * @param date the date
	 * @return the local date time
	 */
	public LocalDateTime formatterDate(Date date) {

		LocalDateTime dateTime = null;
		try {

			dateTime = LocalDateTime.ofInstant(date.toInstant(), ZoneId.systemDefault());
		}
		catch (DateTimeParseException e) {
			e.printStackTrace();
		}
		return dateTime;
	}

}
