/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.io.FileNotFoundException;
import java.io.InputStream;
import java.util.List;

import org.apache.chemistry.opencmis.client.api.Document;
import org.apache.commons.io.IOUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.acm.exceptions.type.GEDException;
import com.acm.service.DocumentService;
import com.acm.utils.dtos.AcmDocumentsDTO;
import com.acm.utils.dtos.GedDocumentDTO;
import com.acm.utils.dtos.GedParameterDTO;
import com.acm.utils.models.AcmDocumentsGed;

/**
 * {@link DocumentController} class.Services relatives to GED manipulation.
 *
 * @author HaythemBenizid
 * @since 0.5.0
 */
@RestController
@RequestMapping("/documents")
public class DocumentController {

	/** The document service. */
	@Autowired
	private DocumentService documentService;

	/**
	 * standard method to upload files to GED using bean {@link GedParameterDTO}.
	 * 
	 * @author HaythemBenizid
	 * @param fileParameters the file parameters
	 * @return List of documents ids {@link String} of the uploaded files
	 * @throws GEDException on GEDException error
	 * @throws FileNotFoundException on FileNotFoundException error
	 */
	@PostMapping(path = "/upload-ged/")
	@ResponseBody
	public List<String> upload(@RequestBody GedParameterDTO fileParameters)
			throws GEDException, FileNotFoundException {

		return documentService.uploadToGed(fileParameters);
	}

	/**
	 * standard method to upload list of files in GED using bean {@link GedParameterDTO}.
	 *
	 * @author HaythemBenizid
	 * @param gedParameterDTOs the ged parameter DT os
	 * @return List of documents ids {@link String} of the uploaded files
	 * @throws GEDException on GEDException error
	 * @throws FileNotFoundException on FileNotFoundException error
	 */
	@PostMapping(path = "/upload-list-ged/")
	@ResponseBody
	public List<GedParameterDTO> upload(@RequestBody List<GedParameterDTO> gedParameterDTOs)
			throws GEDException, FileNotFoundException {

		return documentService.uploadListToGed(gedParameterDTOs);
	}

	/**
	 * standard method to upload files to GED.
	 * 
	 * @author HaythemBenizid
	 * @param files the list of files to be uploaded to GED
	 * @param tags the tags of files
	 * @param path the path to upload
	 * @return List of documents {@link Document} of the uploaded files
	 * @throws FileNotFoundException the file not found exception
	 * @throws GEDException the GED exception
	 */
	@PostMapping(path = "/")
	@ResponseBody
	public List<Document> upload(@RequestParam List<MultipartFile> files,
			@RequestParam List<String> tags, @RequestParam String path)
			throws FileNotFoundException, GEDException {

		return documentService.upload(files, tags, path);
	}

	/**
	 * Find all documents by tag.
	 *
	 * @author HaythemBenizid
	 * @param tag the tag
	 * @return the list
	 * @throws GEDException the GED exception
	 */
	@GetMapping(path = "/find/{tag}")
	public List<GedDocumentDTO> findDemandeDocuments(@PathVariable("tag") String tag)
			throws GEDException {

		return documentService.findByTag(tag);
	}

	/**
	 * Find by given tags.
	 *
	 * @author HaythemBenizid
	 * @param gedParameterDTO the ged parameter DTO
	 * @return the list
	 * @throws GEDException the GED exception
	 */
	@PostMapping("/find-by-tags")
	public List<GedDocumentDTO> findByTags(@RequestBody GedParameterDTO gedParameterDTO)
			throws GEDException {

		return documentService.find(gedParameterDTO.getAll(), gedParameterDTO.getTags());
	}

	/**
	 * Find document by ID using API.
	 * 
	 * @author HaythemBenizid
	 * @param id the Demande id
	 * @return the list
	 * @throws GEDException on error
	 */
	@GetMapping(path = "/find-document/{id}")
	public GedDocumentDTO find(@PathVariable("id") String id) throws GEDException {

		return documentService.find(id);
	}

	/**
	 * Find document by ID.
	 * 
	 * @author HaythemBenizid
	 * @param id the Demande id
	 * @return the list
	 * @throws GEDException on error
	 */
	@GetMapping(path = "/document/{id}")
	public GedDocumentDTO downloadDocument(@PathVariable("id") String id) throws GEDException {

		try {
			Document document = documentService.downloadDocument(id);
			// INIT result
			GedDocumentDTO gedDocumentDTO =
					new GedDocumentDTO(document.getCreationDate().toString(), "TRUE", "FALSE",
							document.getLastModificationDate().toString(), document.getName(), id,
							document.getContentStreamMimeType(), null);
			// Convert & setting byte
			InputStream inputStream = document.getContentStream().getStream();
			gedDocumentDTO.setDocumentContentByte(IOUtils.toByteArray(inputStream));
			// Returning founded data
			return gedDocumentDTO;
		}
		catch (Exception e) {
			// failed to convert stream to Byte
			e.printStackTrace();
			return new GedDocumentDTO();
		}
	}

	/**
	 * Display document by given ID.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @return the byte[] file converted
	 * @throws GEDException the GED exception
	 */
	@GetMapping(path = "/display/{id}")
	public byte[] displayDocument(@PathVariable("id") String id) throws GEDException {

		return documentService.displayDocument(id);
	}

	/**
	 * check if document exist in the ged.
	 * 
	 * @author HaythemBenizid
	 * @param id the document ID
	 * @return true if document exist
	 */
	@GetMapping("/isExist/{id}")
	public Boolean isExist(@PathVariable("id") String id) {

		return documentService.isExist(id);
	}

	/**
	 * Checks if is exist in GED by ID & check if exist in backup Table {@link AcmDocumentsGed} by
	 * ID Document ACM .
	 * 
	 * @author HaythemBenizid
	 * @param acmDocumentsDTO the acm documents DTO
	 * @return the boolean
	 */
	@PostMapping("/isExist-in-ged-acm")
	public Boolean isExist(@RequestBody AcmDocumentsDTO acmDocumentsDTO) {

		return documentService.isExist(acmDocumentsDTO);
	}

	/**
	 * Delete document using his id.
	 * 
	 * @author HaythemBenizid
	 * @param id the idDocument to be deleted
	 * @return true when delete success
	 * @throws GEDException on error
	 */
	@DeleteMapping("/{id}")
	public Boolean delete(@PathVariable("id") String id) throws GEDException {

		return documentService.delete(id);
	}

	/**
	 * Find image for customer.
	 *
	 * @author ManelLamloum
	 * @param tag the tag
	 * @return the ged document DTO
	 * @throws GEDException the GED exception
	 */
	@GetMapping("/find-image-for-customer/{tag}")
	public GedDocumentDTO findImageForCustomer(@PathVariable("tag") String tag)
			throws GEDException {

		return documentService.findImageForCustomer(tag);
	}
}
