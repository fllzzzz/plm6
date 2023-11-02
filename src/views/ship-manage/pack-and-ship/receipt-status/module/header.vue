<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <project-radio-button size="small" v-model="query.projectId" class="filter-item" @change="crud.toQuery" />
      <common-radio-button
        v-model="query.manufactureType"
        :options="manufactureTypeEnum.ENUM"
        showOptionAll
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
      <common-radio-button
        v-model="query.receiptStatus"
        :options="receiptStatusEnum.ENUM"
        showOptionAll
        type="enum"
        size="small"
        class="filter-item"
        @change="
          () => {
            crud.toQuery()
            query.shipmentStatus = undefined
          }
        "
      />
      <component-radio-button
        v-if="typeVal !== packEnum.BOX.V"
        v-model="query.productType"
        :options="packTypeEnum.ENUM"
        :unshowVal="query.projectId ? unValOptions : []"
        showOptionAll
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
      <component-radio-button
        v-if="typeVal === packEnum.BOX.V"
        v-model="query.productType"
        :options="bridgePackTypeEnum.ENUM"
        :disabledVal="[bridgePackTypeEnum.AUXILIARY_MATERIAL.V]"
        showOptionAll
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
      <common-radio-button
        v-model="query.shipmentStatus"
        :options="deliveryReceiptStatusEnum.ENUM"
        showOptionAll
        :unshowVal="query.receiptStatus === receiptStatusEnum.RECEIVED.V ? [deliveryReceiptStatusEnum.DELIVERY.V] : []"
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
      <div
        class="date-div"
        style="
          position: relative;
          padding-left: 115px;
          vertical-align: 0px;
          display: inline-block;
          border: 1px solid #dcdfe6;
          border-radius: 4px;
          height: 31px;
        "
      >
        <common-select
          v-model="query.searchType"
          :options="searchDateTypeEnum.ENUM"
          type="enum"
          class="date-select"
          style="width: 115px; vertical-align: middle"
          @change="handleDateChange"
        />
        <el-date-picker
          v-model="query.deliveryDate"
          type="daterange"
          range-separator=":"
          size="small"
          class="filter-item date-item date-change"
          start-placeholder="开始日期"
          end-placeholder="结束日期"
          style="width: 240px; vertical-align: middle"
          :shortcuts="PICKER_OPTIONS_SHORTCUTS"
          @change="handleDateChange"
        />
      </div>
      <!-- <div style="position:relative;padding-left:115px;vertical-align:middle;display:inline-block;">
        <common-select
          v-model="query.searchType"
          :options="searchDateTypeEnum.ENUM"
          type="enum"
          class="date-select"
          style="width:115px;vertical-align:middle;"
          @change="handleDateChange"
        />
        <el-date-picker
          v-model="query.deliveryDate"
          type="daterange"
          range-separator=":"
          size="small"
          class="filter-item date-item"
          start-placeholder="开始日期"
          end-placeholder="结束日期"
          style="width: 240px;vertical-align:middle;"
          :shortcuts="PICKER_OPTIONS_SHORTCUTS"
          @change="handleDateChange"
        />
      </div> -->
      <!-- <el-date-picker
        v-model="query.receiptDate"
        type="daterange"
        range-separator=":"
        size="small"
        class="filter-item date-item"
        start-placeholder="收货开始日期"
        end-placeholder="收货结束日期"
        style="width: 230px"
        @change="handleReceiptDateChange"
      /> -->
      <div>
        <el-input
          v-model="query.blurry"
          placeholder="产品名称/产品编号"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <el-input
          v-model="query.serialNumber"
          placeholder="可输入车次搜索"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <el-input
          v-model="query.licensePlate"
          placeholder="可输入车牌搜索"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <el-input
          v-model="query.driverName"
          placeholder="可输入司机搜索"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <el-input
          v-model="query.auditUserName"
          placeholder="可输入发运人搜索"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <el-input
          v-model="query.receiptName"
          placeholder="可输入收货人搜索"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <el-input
          v-model="query.auditReceiptName"
          placeholder="可输入实际收货人搜索"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <rrOperation />
      </div>
    </div>
    <crudOperation>
      <template v-slot:optLeft>
        <print-table
          v-permission="[...permission.print, ...permission.detailPrint]"
          v-model:current-key="currentKey"
          :api-key="apiKey"
          :params="printParams"
          :before-print="handleBeforePrint"
          size="mini"
          type="warning"
          class="filter-item"
        />
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { inject, onMounted, ref, computed, watch } from 'vue'
import moment from 'moment'
import { projectTypeEnum } from '@enum-ms/contract'
import { packTypeEnum, receiptStatusEnum, deliveryReceiptStatusEnum, searchDateTypeEnum } from '@enum-ms/mes'
import { packEnum } from '@enum-ms/ship-manage'
import { bridgePackTypeEnum } from '@enum-ms/bridge'
import { manufactureTypeEnum } from '@enum-ms/production'
import { isNotBlank } from '@data-type/index'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'
import checkPermission from '@/utils/system/check-permission'
import { mapGetters } from '@/store/lib'
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import { ElMessage } from 'element-plus'

const typeVal = ref()
const { globalProject } = mapGetters(['globalProject'])
const defaultQuery = {
  serialNumber: undefined,
  licensePlate: undefined,
  driverName: undefined,
  auditUserName: undefined,
  auditStartDate: undefined,
  auditEndDate: undefined,
  auditReceiptName: undefined,
  receiptName: undefined,
  projectId: { value: undefined, resetAble: false },
  productType: { value: undefined, resetAble: false },
  manufactureType: { value: undefined, resetAble: false },
  shipmentStatus: undefined
}
const { crud, query } = regHeader(defaultQuery)

const permission = inject('permission')
const currentKey = ref()
const apiKey = ref([])

watch(
  () => globalProject.value,
  (val) => {
    query.productType = undefined
    typeVal.value = undefined
    typeVal.value = globalProject.value?.productCategory
  },
  { immediate: true }
)

const unValOptions = computed(() => {
  switch (typeVal.value) {
    case packTypeEnum.STRUCTURE.V:
      return [packTypeEnum.ENCLOSURE.V]
    case packTypeEnum.ENCLOSURE.V:
      return [packTypeEnum.STRUCTURE.V, packTypeEnum.MACHINE_PART.V]
    case packTypeEnum.STRUCTURE.V + packTypeEnum.ENCLOSURE.V:
      return []
    default:
      return []
  }
})

onMounted(() => {
  if (checkPermission(permission.print)) {
    crud.query.projectType === projectTypeEnum.BRIDGE.V
      ? apiKey.value.push('mesBridgeReceiptStatusSummary')
      : apiKey.value.push('mesReceiptStatusSummary')
  }
  if (checkPermission(permission.detailPrint)) {
    crud.query.projectType === projectTypeEnum.BRIDGE.V ? apiKey.value.push('mesBridgeShippingList') : apiKey.value.push('mesShippingList')
  }
})

const printParams = computed(() => {
  if (currentKey.value === 'mesReceiptStatusSummary' || currentKey.value === 'mesBridgeReceiptStatusSummary') {
    return { ...query }
  }
  if (
    (currentKey.value === 'mesShippingList' && isNotBlank(crud.selections)) ||
    (currentKey.value === 'mesBridgeShippingList' && isNotBlank(crud.selections))
  ) {
    return crud.selections.map((row) => {
      return row.id
    })
  }
  return undefined
})

function handleBeforePrint() {
  if (
    (currentKey.value === 'mesShippingList' && !isNotBlank(printParams.value)) ||
    (currentKey.value === 'mesBridgeShippingList' && !isNotBlank(printParams.value))
  ) {
    ElMessage.warning('至少选择一条需要打印的发运信息')
    return false
  }
}

function handleDateChange() {
  if (query.deliveryDate && query.deliveryDate.length > 1) {
    if (query.searchType === searchDateTypeEnum.DELIVERY_DATE.V) {
      query.auditStartDate = moment(query.deliveryDate[0]).valueOf()
      query.auditEndDate = moment(query.deliveryDate[1]).valueOf()
      query.statusUpdateStartDate = undefined
      query.statusUpdateEndDate = undefined
    } else {
      query.statusUpdateStartDate = moment(query.deliveryDate[0]).valueOf()
      query.statusUpdateEndDate = moment(query.deliveryDate[1]).valueOf()
      query.auditStartDate = undefined
      query.auditEndDate = undefined
    }
  } else {
    query.statusUpdateStartDate = undefined
    query.statusUpdateEndDate = undefined
    query.auditStartDate = undefined
    query.auditEndDate = undefined
  }
}
</script>
<style lang="scss" scoped>
.date-select {
  position: absolute;
  top: 0;
  left: 3px;
  ::v-deep(.el-input__inner) {
    border: 0 none;
    // border-right:0 none;
    line-height: 29px;
    height: 29px;
    padding-left: 2px;
    padding-right: 32px;
  }
}
::v-deep(.date-change.el-input__inner) {
  border: 0 none;
  line-height: 29px;
  height: 29px;
}
.date-div {
  &::before {
    content: '';
    width: 1px;
    height: 30px;
    position: absolute;
    top: 0;
    left: 116px;
    background-color: #dcdfe6;
    z-index: 500;
  }
}
</style>
