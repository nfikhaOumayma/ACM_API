/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.time.ZoneId;
import java.time.format.DateTimeFormatter;

import javax.activation.DataHandler;
import javax.mail.Message;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeBodyPart;
import javax.mail.internet.MimeMessage;
import javax.mail.internet.MimeMultipart;
import javax.mail.util.ByteArrayDataSource;

import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.stereotype.Service;

import com.acm.service.CalendarSrvice;
import com.acm.utils.dtos.CalendarOutlookRequest;

/**
 * The Class CalendarServiceImpl.
 */
@Service
public class CalendarServiceImpl implements CalendarSrvice {

	/** The mail sender. */
	private JavaMailSender mailSender;

	/**
	 * Instantiates a new calendar service impl.
	 *
	 * @param mailSender the mail sender
	 */
	public CalendarServiceImpl(JavaMailSender mailSender) {

		this.mailSender = mailSender;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.CalendarSrvice#sendCalendarInvite(java.lang.String,
	 * com.acm.utils.dtos.CalendarOutlookRequest)
	 */
	@Override
	public void sendCalendarInvite(String fromEmail, CalendarOutlookRequest calendarRequest)
			throws Exception {

		MimeMessage mimeMessage = mailSender.createMimeMessage();
		mimeMessage.addHeaderLine("method=REQUEST");
		mimeMessage.addHeaderLine("charset=UTF-8");
		mimeMessage.addHeaderLine("component=VEVENT");
		mimeMessage.setFrom(new InternetAddress(fromEmail));
		mimeMessage.addRecipient(Message.RecipientType.TO,
				new InternetAddress(calendarRequest.getToEmail()));
		mimeMessage.setSubject(calendarRequest.getSubject());
		DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyyMMdd HHmmss");
		StringBuilder builder = new StringBuilder();
		builder.append("BEGIN:VCALENDAR\n" + "METHOD:REQUEST\n"
				+ "PRODID:Microsoft Exchange Server 2010\n" + "VERSION:2.0\n" + "BEGIN:VTIMEZONE\n"
				+ "TZID:" + ZoneId.systemDefault().getId() + "\n" + "END:VTIMEZONE\n"
				+ "BEGIN:VEVENT\n" + "ATTENDEE;ROLE=REQ-PARTICIPANT;RSVP=TRUE:MAILTO:"
				+ calendarRequest.getToEmail() + "\n" + "ORGANIZER;CN=Foo:MAILTO:" + fromEmail
				+ "\n" + "DESCRIPTION;LANGUAGE=en-US:" + calendarRequest.getBody() + "\n" + "UID:"
				+ calendarRequest.getUid() + "\n" + "SUMMARY;LANGUAGE=en-US:"
				+ calendarRequest.getSubject() + "\n" + "DTSTART:"
				+ formatter.format(calendarRequest.getMeetingStartTime().minusHours(1)).replace(" ",
						"T")
				+ "\n" + "DTEND:"
				+ formatter.format(calendarRequest.getMeetingEndTime().minusHours(1)).replace(" ",
						"T")
				+ "\n" + "CLASS:PUBLIC\n" + "PRIORITY:5\n" + "DTSTAMP:20230302T082102Z\n"
				+ "TRANSP:OPAQUE\n" + "STATUS:CONFIRMED\n" + "SEQUENCE:$sequenceNumber\n"
				+ "LOCATION:" + calendarRequest.getLocation() + "\n" + "BEGIN:VALARM\n"
				+ "DESCRIPTION:REMINDER\n" + "TRIGGER;RELATED=START:-PT15M\n" + "ACTION:DISPLAY\n"
				+ "END:VALARM\n" + "END:VEVENT\n" + "END:VCALENDAR");

		MimeBodyPart messageBodyPart = new MimeBodyPart();

		messageBodyPart.setHeader("Content-Class", "urn:content-classes:calendarmessage");
		messageBodyPart.setHeader("Content-ID", "calendar_message");
		messageBodyPart.setDataHandler(new DataHandler(new ByteArrayDataSource(builder.toString(),
				"text/calendar;method=REQUEST;name=\"invite.ics\"")));

		MimeMultipart multipart = new MimeMultipart();

		multipart.addBodyPart(messageBodyPart);

		mimeMessage.setContent(multipart);

		mailSender.send(mimeMessage);

	}

}
