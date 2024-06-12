/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.vneuron.utils.dtos;

import java.util.ArrayList;

/**
 * {@link SearchResultItem } class.
 *
 * @author kouali
 * @since 0.1.0
 */
public class SearchResultItem {

	/** The score. */
	public double score;

	/** The name type. */
	public Object nameType;

	/** The action. */
	public Object action;

	/** The tags. */
	public ArrayList<Object> tags;

	/** The reject. */
	public boolean reject;

	/** The msg. */
	public Object msg;

	/** The list types. */
	public ArrayList<String> listTypes;

	/** The full text search result. */
	public boolean fullTextSearchResult;

	/** The custom listed. */
	public boolean customListed;

	/** The sanctionned. */
	public boolean sanctionned;

	/** The pep. */
	public boolean pep;

	/** The search query id. */
	public int search_query_id;

	/** The person id. */
	public Long person_id;

	/** The list name. */
	public String list_name;

	/** The deceased. */
	public Object deceased;

	/** The gender. */
	public Object gender;

	/** The nationality. */
	public ArrayList<Nationality> nationality;

	/** The id in list. */
	public Object id_inList;

	/** The sublists. */
	public ArrayList<Object> sublists;

	/** The name. */
	public ArrayList<Name> name;

	/** The birth date. */
	public ArrayList<BirthDate> birth_date;

	/** The images. */
	public ArrayList<Object> images;

	/** The sub list names. */
	public ArrayList<String> sub_list_names;

	/** The person type. */
	public String person_type;

	/** The has risked country. */
	public boolean hasRiskedCountry;

	/** The is PEP. */
	public boolean isPEP;

	/** The is sanctionned. */
	public boolean isSanctionned;

	/** The is custom listed. */
	public boolean isCustomListed;

	/** The business name. */
	public Object business_name;

	/** The id number info. */
	public ArrayList<IdNumberInfo> id_number_info;

	/** The has adverse media. */
	public boolean hasAdverseMedia;

	/** The full text search. */
	public Object full_text_search;

	/** The search type. */
	public Object search_Type;

	/** The further information. */
	public Object further_information;

	/** The full text search result. */
	public boolean full_text_search_result;

	/**
	 * Gets the score.
	 *
	 * @return the score
	 */
	public double getScore() {

		return score;
	}

	/**
	 * Sets the score.
	 *
	 * @param score the score to set
	 */
	public void setScore(double score) {

		this.score = score;
	}

	/**
	 * Gets the name type.
	 *
	 * @return the nameType
	 */
	public Object getNameType() {

		return nameType;
	}

	/**
	 * Sets the name type.
	 *
	 * @param nameType the nameType to set
	 */
	public void setNameType(Object nameType) {

		this.nameType = nameType;
	}

	/**
	 * Gets the action.
	 *
	 * @return the action
	 */
	public Object getAction() {

		return action;
	}

	/**
	 * Sets the action.
	 *
	 * @param action the action to set
	 */
	public void setAction(Object action) {

		this.action = action;
	}

	/**
	 * Gets the tags.
	 *
	 * @return the tags
	 */
	public ArrayList<Object> getTags() {

		return tags;
	}

	/**
	 * Sets the tags.
	 *
	 * @param tags the tags to set
	 */
	public void setTags(ArrayList<Object> tags) {

		this.tags = tags;
	}

	/**
	 * Checks if is reject.
	 *
	 * @return the reject
	 */
	public boolean isReject() {

		return reject;
	}

	/**
	 * Sets the reject.
	 *
	 * @param reject the reject to set
	 */
	public void setReject(boolean reject) {

		this.reject = reject;
	}

	/**
	 * Gets the msg.
	 *
	 * @return the msg
	 */
	public Object getMsg() {

		return msg;
	}

	/**
	 * Sets the msg.
	 *
	 * @param msg the msg to set
	 */
	public void setMsg(Object msg) {

		this.msg = msg;
	}

	/**
	 * Gets the list types.
	 *
	 * @return the listTypes
	 */
	public ArrayList<String> getListTypes() {

		return listTypes;
	}

	/**
	 * Sets the list types.
	 *
	 * @param listTypes the listTypes to set
	 */
	public void setListTypes(ArrayList<String> listTypes) {

		this.listTypes = listTypes;
	}

	/**
	 * Checks if is full text search result.
	 *
	 * @return the fullTextSearchResult
	 */
	public boolean isFullTextSearchResult() {

		return fullTextSearchResult;
	}

	/**
	 * Sets the full text search result.
	 *
	 * @param fullTextSearchResult the fullTextSearchResult to set
	 */
	public void setFullTextSearchResult(boolean fullTextSearchResult) {

		this.fullTextSearchResult = fullTextSearchResult;
	}

	/**
	 * Checks if is custom listed.
	 *
	 * @return the customListed
	 */
	public boolean isCustomListed() {

		return customListed;
	}

	/**
	 * Sets the custom listed.
	 *
	 * @param customListed the customListed to set
	 */
	public void setCustomListed(boolean customListed) {

		this.customListed = customListed;
	}

	/**
	 * Checks if is sanctionned.
	 *
	 * @return the sanctionned
	 */
	public boolean isSanctionned() {

		return sanctionned;
	}

	/**
	 * Sets the sanctionned.
	 *
	 * @param sanctionned the sanctionned to set
	 */
	public void setSanctionned(boolean sanctionned) {

		this.sanctionned = sanctionned;
	}

	/**
	 * Checks if is pep.
	 *
	 * @return the pep
	 */
	public boolean getPep() {

		return pep;
	}

	/**
	 * Sets the pep.
	 *
	 * @param pep the pep to set
	 */
	public void setPep(boolean pep) {

		this.pep = pep;
	}

	/**
	 * Gets the search query id.
	 *
	 * @return the search_query_id
	 */
	public int getSearch_query_id() {

		return search_query_id;
	}

	/**
	 * Sets the search query id.
	 *
	 * @param search_query_id the search_query_id to set
	 */
	public void setSearch_query_id(int search_query_id) {

		this.search_query_id = search_query_id;
	}

	/**
	 * Gets the person id.
	 *
	 * @return the person_id
	 */
	public Long getPerson_id() {

		return person_id;
	}

	/**
	 * Sets the person id.
	 *
	 * @param person_id the person_id to set
	 */
	public void setPerson_id(Long person_id) {

		this.person_id = person_id;
	}

	/**
	 * Gets the list name.
	 *
	 * @return the list_name
	 */
	public String getList_name() {

		return list_name;
	}

	/**
	 * Sets the list name.
	 *
	 * @param list_name the list_name to set
	 */
	public void setList_name(String list_name) {

		this.list_name = list_name;
	}

	/**
	 * Gets the deceased.
	 *
	 * @return the deceased
	 */
	public Object getDeceased() {

		return deceased;
	}

	/**
	 * Sets the deceased.
	 *
	 * @param deceased the deceased to set
	 */
	public void setDeceased(Object deceased) {

		this.deceased = deceased;
	}

	/**
	 * Gets the gender.
	 *
	 * @return the gender
	 */
	public Object getGender() {

		return gender;
	}

	/**
	 * Sets the gender.
	 *
	 * @param gender the gender to set
	 */
	public void setGender(Object gender) {

		this.gender = gender;
	}

	/**
	 * Gets the nationality.
	 *
	 * @return the nationality
	 */
	public ArrayList<Nationality> getNationality() {

		return nationality;
	}

	/**
	 * Sets the nationality.
	 *
	 * @param nationality the nationality to set
	 */
	public void setNationality(ArrayList<Nationality> nationality) {

		this.nationality = nationality;
	}

	/**
	 * Gets the id in list.
	 *
	 * @return the id_inList
	 */
	public Object getId_inList() {

		return id_inList;
	}

	/**
	 * Sets the id in list.
	 *
	 * @param id_inList the id_inList to set
	 */
	public void setId_inList(Object id_inList) {

		this.id_inList = id_inList;
	}

	/**
	 * Gets the sublists.
	 *
	 * @return the sublists
	 */
	public ArrayList<Object> getSublists() {

		return sublists;
	}

	/**
	 * Sets the sublists.
	 *
	 * @param sublists the sublists to set
	 */
	public void setSublists(ArrayList<Object> sublists) {

		this.sublists = sublists;
	}

	/**
	 * Gets the name.
	 *
	 * @return the name
	 */
	public ArrayList<Name> getName() {

		return name;
	}

	/**
	 * Sets the name.
	 *
	 * @param name the name to set
	 */
	public void setName(ArrayList<Name> name) {

		this.name = name;
	}

	/**
	 * Gets the birth date.
	 *
	 * @return the birth_date
	 */
	public ArrayList<BirthDate> getBirth_date() {

		return birth_date;
	}

	/**
	 * Sets the birth date.
	 *
	 * @param birth_date the birth_date to set
	 */
	public void setBirth_date(ArrayList<BirthDate> birth_date) {

		this.birth_date = birth_date;
	}

	/**
	 * Gets the images.
	 *
	 * @return the images
	 */
	public ArrayList<Object> getImages() {

		return images;
	}

	/**
	 * Sets the images.
	 *
	 * @param images the images to set
	 */
	public void setImages(ArrayList<Object> images) {

		this.images = images;
	}

	/**
	 * Gets the sub list names.
	 *
	 * @return the sub_list_names
	 */
	public ArrayList<String> getSub_list_names() {

		return sub_list_names;
	}

	/**
	 * Sets the sub list names.
	 *
	 * @param sub_list_names the sub_list_names to set
	 */
	public void setSub_list_names(ArrayList<String> sub_list_names) {

		this.sub_list_names = sub_list_names;
	}

	/**
	 * Gets the person type.
	 *
	 * @return the person_type
	 */
	public String getPerson_type() {

		return person_type;
	}

	/**
	 * Sets the person type.
	 *
	 * @param person_type the person_type to set
	 */
	public void setPerson_type(String person_type) {

		this.person_type = person_type;
	}

	/**
	 * Checks if is checks for risked country.
	 *
	 * @return the hasRiskedCountry
	 */
	public boolean isHasRiskedCountry() {

		return hasRiskedCountry;
	}

	/**
	 * Sets the checks for risked country.
	 *
	 * @param hasRiskedCountry the hasRiskedCountry to set
	 */
	public void setHasRiskedCountry(boolean hasRiskedCountry) {

		this.hasRiskedCountry = hasRiskedCountry;
	}

	/**
	 * Checks if is pep.
	 *
	 * @return the isPEP
	 */
	public boolean getIsPEP() {

		return isPEP;
	}

	/**
	 * Sets the pep.
	 *
	 * @param isPEP the isPEP to set
	 */
	public void setIsPEP(boolean isPEP) {

		this.isPEP = isPEP;
	}

	/**
	 * Gets the business name.
	 *
	 * @return the business_name
	 */
	public Object getBusiness_name() {

		return business_name;
	}

	/**
	 * Sets the business name.
	 *
	 * @param business_name the business_name to set
	 */
	public void setBusiness_name(Object business_name) {

		this.business_name = business_name;
	}

	/**
	 * Gets the id number info.
	 *
	 * @return the id_number_info
	 */
	public ArrayList<IdNumberInfo> getId_number_info() {

		return id_number_info;
	}

	/**
	 * Sets the id number info.
	 *
	 * @param id_number_info the id_number_info to set
	 */
	public void setId_number_info(ArrayList<IdNumberInfo> id_number_info) {

		this.id_number_info = id_number_info;
	}

	/**
	 * Checks if is checks for adverse media.
	 *
	 * @return the hasAdverseMedia
	 */
	public boolean isHasAdverseMedia() {

		return hasAdverseMedia;
	}

	/**
	 * Sets the checks for adverse media.
	 *
	 * @param hasAdverseMedia the hasAdverseMedia to set
	 */
	public void setHasAdverseMedia(boolean hasAdverseMedia) {

		this.hasAdverseMedia = hasAdverseMedia;
	}

	/**
	 * Gets the full text search.
	 *
	 * @return the full_text_search
	 */
	public Object getFull_text_search() {

		return full_text_search;
	}

	/**
	 * Sets the full text search.
	 *
	 * @param full_text_search the full_text_search to set
	 */
	public void setFull_text_search(Object full_text_search) {

		this.full_text_search = full_text_search;
	}

	/**
	 * Gets the search type.
	 *
	 * @return the search_Type
	 */
	public Object getSearch_Type() {

		return search_Type;
	}

	/**
	 * Sets the search type.
	 *
	 * @param search_Type the search_Type to set
	 */
	public void setSearch_Type(Object search_Type) {

		this.search_Type = search_Type;
	}

	/**
	 * Gets the further information.
	 *
	 * @return the further_information
	 */
	public Object getFurther_information() {

		return further_information;
	}

	/**
	 * Sets the further information.
	 *
	 * @param further_information the further_information to set
	 */
	public void setFurther_information(Object further_information) {

		this.further_information = further_information;
	}

	/**
	 * Checks if is full text search result.
	 *
	 * @return the full_text_search_result
	 */
	public boolean isFull_text_search_result() {

		return full_text_search_result;
	}

	/**
	 * Sets the full text search result.
	 *
	 * @param full_text_search_result the full_text_search_result to set
	 */
	public void setFull_text_search_result(boolean full_text_search_result) {

		this.full_text_search_result = full_text_search_result;
	}

}
