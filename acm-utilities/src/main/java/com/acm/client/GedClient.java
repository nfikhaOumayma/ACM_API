/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.client;

import java.io.FileNotFoundException;
import java.util.List;

import org.springframework.cloud.netflix.ribbon.RibbonClient;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.ResponseBody;

import com.acm.configuration.feignclient.ClientConfiguration;
import com.acm.configuration.feignclient.LoadbalancerRuleFeignConfiguration;
import com.acm.utils.dtos.AcmDocumentsDTO;
import com.acm.utils.dtos.GedDocumentDTO;
import com.acm.utils.dtos.GedParameterDTO;
import com.acm.utils.models.AcmDocumentsGed;

/**
 * {@link GedClient} class.Clonage de module Ged pour upload des fichiers.
 *
 * @author HaythemBenizid
 * @since 0.5.0
 */
@FeignClient(value = "ged-service", configuration = ClientConfiguration.class, decode404 = true)
@RibbonClient(name = "ged-service", configuration = LoadbalancerRuleFeignConfiguration.class)
public interface GedClient {

	/**
	 * upload document to GED.
	 *
	 * @author HaythemBenizid
	 * @param fileParameters the list of files to be uploaded
	 * @return the List of uploaded Document
	 * @throws FileNotFoundException the file not found exception
	 */
	@PostMapping(path = "/documents/upload-ged/")
	@ResponseBody
	List<String> uploadToGed(@RequestBody GedParameterDTO fileParameters)
			throws FileNotFoundException;

	/**
	 * Upload list to GED.
	 *
	 * @author HaythemBenizid
	 * @param gedParameterDTOs the ged parameter DT os
	 * @return the List of uploaded Document
	 * @throws FileNotFoundException the file not found exception
	 */
	@PostMapping(path = "/documents/upload-list-ged/")
	@ResponseBody
	List<GedParameterDTO> uploadListToGed(@RequestBody List<GedParameterDTO> gedParameterDTOs)
			throws FileNotFoundException;

	/**
	 * find document by id.
	 * 
	 * @author HaythemBenizid
	 * @param id the id document
	 * @return the {@link DocumentDTO }
	 */
	@GetMapping(path = "/documents/document/{id}")
	GedDocumentDTO find(@PathVariable("id") String id);

	/**
	 * Find demande documents.
	 * 
	 * @author HaythemBenizid
	 * @author AsmaBenAmor
	 * @param tag the tag
	 * @return the list
	 */
	@GetMapping(path = "/documents/find/{tag}")
	List<GedDocumentDTO> findDemandeDocuments(@PathVariable("tag") String tag);

	/**
	 * Checks if is exist in GED by ID & check if exist in backup Table {@link AcmDocumentsGed} by
	 * ID Document ACM .
	 * 
	 * @author HaythemBenizid
	 * @param acmDocumentsDTO the acm documents DTO
	 * @return the boolean
	 */
	@PostMapping("/documents/isExist-in-ged-acm")
	Boolean isExist(@RequestBody AcmDocumentsDTO acmDocumentsDTO);

	/**
	 * Delete document using his id.
	 *
	 * @author AbdelkarimTurki
	 * @param id the idDocument to be deleted
	 * @return true when delete success
	 */
	@DeleteMapping("/documents/{id}")
	Boolean delete(@PathVariable("id") String id);

	/**
	 * Find photo client.
	 * 
	 * @author idridi
	 * @param idDocument the id document
	 * @return the byte[]
	 */
	@GetMapping(path = "/documents-ged/photo-client/{idDocument}")
	byte[] findPhotoClient(@PathVariable("idDocument") Long idDocument);

	/**
	 * Display document.
	 *
	 * @param id the id
	 * @return the byte[]
	 */
	@GetMapping(path = "/documents/display/{id}")
	byte[] displayDocument(@PathVariable("id") String id);
}
