package com.acm.controller;

import java.io.IOException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.service.SmsSenderService;
import com.acm.utils.dtos.MessageDetailsDTO;

/**
 * {@link SmsSenderController} class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
@RestController
@RequestMapping("/sms-sender")
public class SmsSenderController {

	/** The sms sender service. */
	@Autowired
	private SmsSenderService smsSenderService;

	/**
	 * Send sms.
	 *
	 * @param messageDetailsDTO the message details DTO
	 * @return the string
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	@PostMapping("/send-sms")
	public String sendSms(@RequestBody MessageDetailsDTO messageDetailsDTO) throws IOException {

		return smsSenderService.sendSms(messageDetailsDTO.getToSender(),
				messageDetailsDTO.getMessageBody());
	}

	/**
	 * Save SMS.
	 *
	 * @param messageDetailsDTO the message details DTO
	 * @return the message details DTO
	 */
	@PostMapping("/save-sms")
	public MessageDetailsDTO saveSMS(@RequestBody MessageDetailsDTO messageDetailsDTO) {

		return smsSenderService.saveSms(messageDetailsDTO);
	}
}
