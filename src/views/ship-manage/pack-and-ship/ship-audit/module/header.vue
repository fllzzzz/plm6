<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <project-radio-button size="small" v-model="query.projectId" class="filter-item" @change="crud.toQuery" />
      <common-radio-button
        v-model="query.checkStatus"
        :options="shipAuditStatusEnum.ENUM"
        showOptionAll
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
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
      <div>
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
          v-model="query.userName"
          placeholder="可输入装车人搜索"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <el-input
          v-model="query.actualUserName"
          placeholder="可输入过磅人搜索"
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
          :api-key="
            query.checkStatus === shipAuditStatusEnum.UNCHECKED.V
              ? apiKey
              : crud.query.projectType === projectTypeEnum.BRIDGE.V
              ? 'mesBridgeShipmentAudit'
              : 'mesShipmentAudit'
          "
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
import { packTypeEnum, shipAuditStatusEnum } from '@enum-ms/mes'
import { ref, inject, computed, onMounted, watch } from 'vue'
import { mapGetters } from '@/store/lib'
import { projectTypeEnum } from '@enum-ms/contract'
import { packEnum } from '@enum-ms/ship-manage'
import { bridgePackTypeEnum } from '@enum-ms/bridge'
import { regHeader } from '@compos/use-crud'
import { isBlank, isNotBlank } from '@data-type/index'
import { ElMessage } from 'element-plus'
import checkPermission from '@/utils/system/check-permission'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const typeVal = ref()
const currentKey = ref()
const apiKey = ref([])

const defaultQuery = {
  serialNumber: undefined,
  licensePlate: undefined,
  userName: undefined,
  actualUserName: undefined,
  projectId: { value: undefined, resetAble: false },
  productType: { value: undefined, resetAble: false }
}
const { crud, query } = regHeader(defaultQuery)

const { globalProject } = mapGetters(['globalProject'])
const permission = inject('permission')
onMounted(() => {
  if (checkPermission(permission.print)) {
    crud.query.projectType === projectTypeEnum.BRIDGE.V
      ? apiKey.value.push('mesBridgeShipmentAudit')
      : apiKey.value.push('mesShipmentAudit')
  }
  if (checkPermission(permission.detailPrint)) {
    crud.query.projectType === projectTypeEnum.BRIDGE.V
      ? apiKey.value.push('mesBridgeShipmentAuditOverWeight')
      : apiKey.value.push('mesShipmentAuditOverWeight')
  }
})

const printParams = computed(() => {
  if (currentKey.value === 'mesShipmentAudit' || currentKey.value === 'mesBridgeShipmentAudit') {
    return { ...query }
  }
  if (
    (currentKey.value === 'mesShipmentAuditOverWeight' && isNotBlank(crud.selections)) ||
    (currentKey.value === 'mesBridgeShipmentAuditOverWeight' && isNotBlank(crud.selections))
  ) {
    return crud.selections.map((row) => {
      return row.id
    })
  }
  return undefined
})

function handleBeforePrint() {
  if (
    (currentKey.value === 'mesShipmentAuditOverWeight' && isBlank(printParams.value)) ||
    (currentKey.value === 'mesBridgeShipmentAuditOverWeight' && isBlank(printParams.value))
  ) {
    ElMessage.warning('至少选择一条需要打印的过磅信息')
    return false
  }
}

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
</script>
