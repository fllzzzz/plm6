<template>
  <div class="head-container">
    <div>
      <el-date-picker
        v-model="query.date"
        type="daterange"
        range-separator="-"
        value-format="x"
        start-placeholder="开始时间"
        end-placeholder="结束时间"
        class="filter-item"
        @change="handleDateChange"
      />
      <common-select
        placeholder="请选择类型"
        class="filter-item"
        :options="scrapTypeList"
        :data-structure="{key: 'id', label: 'name', value: 'id'}"
        v-model="query.wasteClassificationId"
        clearable
        @change="crud.toQuery"
      />
    </div>
    <div style="display: flex; justify-content: space-between;">
      <div>
        <common-button icon="el-icon-plus" type="success" class="filter-item" size="mini" @click="creatScrap">创建</common-button>
        <el-input style="width: 200px;" class="filter-item" placeholder="购买方搜索" v-model="query.purchaser"  />
        <el-input style="width: 200px;" class="filter-item" placeholder="创建人搜索" v-model="query.createUserName" />
        <el-input style="width: 200px;" class="filter-item" placeholder="审核人搜索" v-model="query.auditUserName" />
        <rrOperation />
      </div>
      <print-table class="filter-item" api-key="wmsScrapSell" />
    </div>
    <createDetaile v-model="creatDetail" :typeList="scrapTypeList" @success="success" />
  </div>
</template>
<script setup>
import { ref } from 'vue'
import { getScrapTypeList } from '@/api/contract/scrap-ledger'
import { regHeader } from '@compos/use-crud'
import createDetaile from './detail.vue'
import rrOperation from '@crud/RR.operation'
import moment from 'moment'

const scrapTypeList = ref([])
const creatDetail = ref(false)
const defaultQuery = {
  purchaser: undefined,
  createUserName: undefined,
  auditUserName: undefined,
  wasteClassificationId: undefined,
  date: [moment().startOf('month').valueOf(), moment().valueOf()],
  startDate: moment().startOf('month').valueOf(),
  endDate: moment().valueOf()
}

const { crud, query } = regHeader(defaultQuery)

fetchScrapType()

async function fetchScrapType() {
  try {
    const { content } = await getScrapTypeList()
    console.log(content)
    scrapTypeList.value = content
  } catch (error) {
    console.log(error)
  }
}

function creatScrap() {
  creatDetail.value = true
}

function success() {
  crud.toQuery()
}

function handleDateChange(v) {
  if (query.date && query.date.length > 1) {
    query.startDate = query.date[0]
    query.endDate = query.date[1]
  } else {
    query.startDate = undefined
    query.endDate = undefined
  }
  crud.toQuery()
}

</script>
