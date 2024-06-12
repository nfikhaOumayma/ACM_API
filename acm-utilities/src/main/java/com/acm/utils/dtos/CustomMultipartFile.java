/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;

import org.springframework.http.MediaType;
import org.springframework.web.multipart.MultipartFile;

/**
 * {@link CustomMultipartFile } class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
public class CustomMultipartFile implements MultipartFile {

	/** The input. */
	private byte[] input;

	/**
	 * Instantiates a new custom multipart file.
	 *
	 * @param input the input
	 */
	public CustomMultipartFile(byte[] input) {

		this.input = input;
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.web.multipart.MultipartFile#getName()
	 */
	@Override
	public String getName() {

		return "HI";
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.web.multipart.MultipartFile#getOriginalFilename()
	 */
	@Override
	public String getOriginalFilename() {

		return "The name";
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.web.multipart.MultipartFile#getContentType()
	 */
	@Override
	public String getContentType() {

		return MediaType.MULTIPART_FORM_DATA_VALUE;
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.web.multipart.MultipartFile#isEmpty()
	 */
	// previous methods
	@Override
	public boolean isEmpty() {

		return input == null || input.length == 0;
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.web.multipart.MultipartFile#getSize()
	 */
	@Override
	public long getSize() {

		return input.length;
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.web.multipart.MultipartFile#getBytes()
	 */
	@Override
	public byte[] getBytes() throws IOException {

		return input;
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.web.multipart.MultipartFile#getInputStream()
	 */
	@Override
	public InputStream getInputStream() throws IOException {

		return new ByteArrayInputStream(input);
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.web.multipart.MultipartFile#transferTo(java.io.File)
	 */
	@Override
	public void transferTo(File destination) throws IOException, IllegalStateException {

		try (FileOutputStream fos = new FileOutputStream(destination)) {
			fos.write(input);
		}
	}
}
