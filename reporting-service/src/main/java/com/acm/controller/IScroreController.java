/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.io.IOException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.service.IScroeFileService;

/**
 * {@link IScroreController} class.
 *
 * @author YesserSomai
 * @since 1.0.5
 */
@RestController
@RequestMapping("/i-score")
public class IScroreController {

	/** The i scroe file service. */
	@Autowired
	private IScroeFileService iScroeFileService;

	/**
	 * Generate file.
	 *
	 * @author YesserSomai
	 * @param startDate the start date
	 * @param endDate the end date
	 * @return the response entity
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	@GetMapping("/file/{startDate}/{endDate}")
	public ResponseEntity<byte[]> generateFile(@PathVariable("startDate") String startDate,
			@PathVariable("endDate") String endDate) throws IOException {

		System.out.println("Generating I-SCORE.dlt File between " + startDate + " and " + endDate);
		// Set the request header
		HttpHeaders headers = new HttpHeaders();
		headers.add("Content-Disposition", "attchement;filename=i-score.dlt");
		headers.setContentType(MediaType.APPLICATION_JSON_UTF8);
		// Set request status
		HttpStatus statusCode = HttpStatus.OK;
		byte[] iscorebytes = iScroeFileService.generateFile(startDate, endDate);
		System.out.println("Sending I-SCORE.dlt File");
		return new ResponseEntity<>(iscorebytes, headers, statusCode);
	}

	/**
	 * Gets the reject file.
	 *
	 * @return the reject file
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	@GetMapping("/rejectfile")
	public ResponseEntity<byte[]> getRejectFile() throws IOException {

		// Set the request header
		HttpHeaders headers = new HttpHeaders();
		headers.add("Content-Disposition", "attchement;filename=i-score.reject");
		headers.setContentType(MediaType.APPLICATION_JSON_UTF8);
		// Set request status
		HttpStatus statusCode = HttpStatus.OK;
		return new ResponseEntity<>(iScroeFileService.getRejectFile(), headers, statusCode);
	}
}
