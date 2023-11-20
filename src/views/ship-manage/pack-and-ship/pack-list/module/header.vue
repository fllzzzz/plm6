<template>
  <div v-show="crud.searchToggle">
    <project-radio-button size="small" v-model="query.projectId" class="filter-item" @change="crud.toQuery" />
    <component-radio-button
      v-if="typeVal !== packEnum.BOX.V"
      v-model="query.productType"
      :options="packTypeEnum.ENUM"
      :unshowVal="query.projectId ? unValOptions : []"
      default
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
      default
      type="enum"
      size="small"
      class="filter-item"
      @change="crud.toQuery"
    />
    <!-- <common-radio-button
      v-if="query.productType & packTypeEnum.ENCLOSURE.V"
      v-model="query.category"
      :options="mesEnclosureTypeEnum.ENUM"
      showOptionAll
      type="enum"
      size="small"
      class="filter-item"
      @change="crud.toQuery"
    /> -->
    <el-date-picker
      v-model="query.date"
      type="daterange"
      unlink-panels
      range-separator="至"
      start-placeholder="开始日期"
      end-placeholder="结束日期"
      size="small"
      class="filter-item"
      style="width: 330px"
      clearable
      :shortcuts="PICKER_OPTIONS_SHORTCUTS"
      @change="handleDateChange"
    />
    <el-input
      v-model="query.serialNumber"
      placeholder="输入包单搜索"
      class="filter-item"
      style="width: 180px"
      size="small"
      clearable
      @keyup.enter="crud.toQuery"
    />
    <el-input
      v-model="query.userName"
      placeholder="输入打包人名称搜索"
      class="filter-item"
      style="width: 180px"
      size="small"
      clearable
      @keyup.enter="crud.toQuery"
    />
    <el-input
      v-model="query.remark"
      placeholder="输入备注搜索"
      class="filter-item"
      style="width: 140px"
      size="small"
      clearable
      @keyup.enter="crud.toQuery"
    />
    <el-input
      v-model="query.productSerialNumber"
      placeholder="输入产品编号搜索"
      class="filter-item"
      style="width: 160px"
      size="small"
      clearable
      @keyup.enter="crud.toQuery"
    />
    <rrOperation />
  </div>
  <crudOperation>
    <template #optRight>
      <common-radio-button class="filter-item" :options="unitOptions" :dataStructure="{label: 'label', key: 'value', value: 'value'}" v-model="unitValue" @change="unitChange" />
      <el-popover placement="bottom-start" width="400" trigger="click">
        <el-form ref="form" :model="printConfig">
          <!-- <el-form-item label="制造商名称">
              <el-input v-model="printConfig.manufacturerName" size="small" style="width:250px" />
            </el-form-item> -->
          <el-form-item label="份数">
            <el-input-number
              v-model="printConfig.copies"
              :step="1"
              :min="1"
              size="small"
              style="width: 250px"
              @change="handleCopiesChange"
            />
          </el-form-item>
          <el-form-item label="显示">
            <el-checkbox v-model="printConfig.showMaterial" label="材质" @change="checkboxMaterial" />
            <el-checkbox v-model="printConfig.showWidth" label="重量" @change="checkboxWidth" />
          </el-form-item>
        </el-form>
        <template #reference>
          <common-button type="primary" size="mini">标签打印设置</common-button>
        </template>
      </el-popover>
      <el-tag v-permission="permission.print" hit effect="plain" size="medium" style="margin-left: 5px">{{
        `份数：${printConfig.copies}`
      }}</el-tag>
      <print-table
        v-permission="permission.printPackList"
        :api-key="
          (crud.query.projectType === projectTypeEnum.ENCLOSURE.V ? 'enclosurePackingList' : crud,
          query.projectType === projectTypeEnum.BRIDGE.V ? 'mesBridgePackingList' : 'mesPackingList')
        "
        :params="printParams"
        :before-print="handleBeforePrint"
        size="mini"
        type="warning"
        class="filter-item"
        style="margin-left: 5px !important"
      />
    </template>
    <template #viewLeft>
      <common-button
        v-permission="permission.print"
        type="success"
        size="mini"
        :disabled="crud.selections.length === 0"
        @click="batchPrint(crud.selections)"
        >批量打印标签</common-button
      >
    </template>
  </crudOperation>
</template>

<script setup>
import { detail, detailBridge } from '@/api/ship-manage/pack-and-ship/pack-list'
import { packageRecordAdd, packageBridgeRecordAdd } from '@/api/mes/label-print/print-record'
import { ref, inject, reactive, defineExpose, computed, defineEmits, watch } from 'vue'
import { mapGetters } from '@/store/lib'
import moment from 'moment'
import { projectTypeEnum } from '@enum-ms/contract'
import { packTypeEnum } from '@enum-ms/mes'
import { packEnum } from '@enum-ms/ship-manage'
import { bridgePackTypeEnum } from '@enum-ms/bridge'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'
import { printPackageLabel } from '@/utils/print/index'
import { QR_SCAN_F_TYPE, QR_SCAN_TYPE } from '@/settings/config'
import { DP } from '@/settings/config'
import { isNotBlank } from '@data-type/index'

import usePrintLabel from '@compos/mes/label-print/use-label-print'
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import { ElMessage } from 'element-plus'

const typeVal = ref()
const unitValue = ref(1)
// const showMaterial = ref(true)
// const showWidth = ref(true)
const emit = defineEmits(['getDetail', 'changeUnit', 'checkboxMaterial', 'checkboxWidth'])
const unitOptions = ref([
  {
    label: '核算单位',
    value: 1
  },
  {
    label: '计量单位',
    value: 2
  }
])

const permission = inject('permission')
const { user, globalProject } = mapGetters(['user', 'globalProject'])
const defaultQuery = {
  serialNumber: undefined,
  userName: undefined,
  startDate: undefined,
  endDate: undefined,
  remark: void 0,
  productSerialNumber: undefined,
  materialTypeArr: { value: undefined, resetAble: false },
  projectId: { value: undefined, resetAble: false }
}
const { crud, query } = regHeader(defaultQuery)

const printConfig = reactive({
  manufacturerName: user.value.companyName,
  copies: 1,
  showMaterial: true,
  showWidth: true
})

const printParams = computed(() => {
  if (isNotBlank(crud.selections)) {
    return crud.selections.map((row) => {
      return row.id
    })
  }
  return undefined
})

const unitChange = (v) => {
  unitValue.value = v
  emit('changeUnit', v)
}

function handleBeforePrint() {
  if (!isNotBlank(printParams.value)) {
    ElMessage.warning('至少选择一条需要打印的包单信息')
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
      return [packTypeEnum.STRUCTURE.V, packTypeEnum.MACHINE_PART.v]
    case packTypeEnum.STRUCTURE.V + packTypeEnum.ENCLOSURE.V:
      return []
    default:
      return []
  }
})

const { batchPrint, print } = usePrintLabel({
  getPrintTotalNumber: () => computed(() => printConfig.copies).value,
  getLabelInfo: getLabelInfo,
  printFinallyHook: crud.toQuery,
  getLoadingTextFunc: (row) => `${row.serialNumber}`,
  printLabelFunc: printPackageLabel,
  needAddPrintRecord: true,
  addPrintRecordReq: crud.query.projectType === projectTypeEnum.BRIDGE.V ? packageBridgeRecordAdd : packageRecordAdd
})

const detailStore = inject('detailStore')
const dataField = {
  [packTypeEnum.STRUCTURE.V]: 'artifactList',
  [packTypeEnum.MACHINE_PART.V]: 'partList',
  [packTypeEnum.ENCLOSURE.V]: 'enclosureList',
  [packTypeEnum.AUXILIARY_MATERIAL.V]: 'auxiliaryMaterialList'
}

async function getLabelInfo(row) {
  const _list = []
  const _auxList = []
  const _structureList = []
  let _data = {}
  try {
    if (detailStore[row.id]) {
      _data = detailStore[row.id]
      console.log(_data, '_data')
    } else {
      _data = crud.query.projectType === projectTypeEnum.BRIDGE.V ? await detailBridge(row.id) : await detail(row.id)
      emit('getDetail', row.id, _data)
    }
    const auxList = _data[dataField[packTypeEnum.AUXILIARY_MATERIAL.V]]
    const structureList = _data[dataField[packTypeEnum.STRUCTURE.V]]
    // 多类型打包处理

    if (auxList?.length > 0 && structureList?.length > 0) {
      for (let m = 0; m < auxList.length; m++) {
        const a = auxList[m]
        const { name, specification, measureUnit, accountingUnit, packageMete, serialNumber, material, packageQuantity, grossWeight, plate, length } = a
        _auxList.push({
          serialNumber,
          name,
          specification,
          measureUnit,
          accountingUnit,
          packageMete,
          material,
          quantity: packageQuantity,
          totalWeight: (packageQuantity * grossWeight).toFixed(DP.COM_WT__KG),
          // totalNetWeight: totalNetWeight ? totalNetWeight.toFixed(DP.COM_WT__KG) : 0,
          plate,
          length: length ? length.toFixed(DP.MES_ENCLOSURE_L__MM) : 0
        })
      }
      for (let p = 0; p < structureList.length; p++) {
        const s = structureList[p]
        const { name, specification, measureUnit, serialNumber, material, packageQuantity, grossWeight, plate, length } = s
        _structureList.push({
          serialNumber,
          name,
          specification,
          measureUnit,
          material,
          quantity: packageQuantity,
          totalWeight: (packageQuantity * grossWeight).toFixed(DP.COM_WT__KG),
          // totalNetWeight: totalNetWeight ? totalNetWeight.toFixed(DP.COM_WT__KG) : 0,
          plate,
          length: length ? length.toFixed(DP.MES_ENCLOSURE_L__MM) : 0
        })
      }
    } else {
      for (const item in dataField) {
        const _itemList = _data[dataField[item]]
        if (_itemList?.length) {
          for (let i = 0; i < _itemList.length; i++) {
            const v = _itemList[i]
            const {
              name,
              specification,
              measureUnit,
              accountingUnit, packageMete,
              serialNumber,
              material,
              packageQuantity,
              grossWeight,
              plate,
              length,
              productType,
              surfaceArea
            } = v
            _list.push({
              serialNumber,
              name,
              specification,
              measureUnit,
              accountingUnit, packageMete,
              material,
              quantity: packageQuantity,
              totalWeight: (packageQuantity * grossWeight).toFixed(DP.COM_WT__KG),
              // totalNetWeight: totalNetWeight ? totalNetWeight.toFixed(DP.COM_WT__KG) : 0,
              plate,
              surfaceArea,
              productType,
              length: length ? length.toFixed(DP.MES_ENCLOSURE_L__MM) : 0
            })
          }
        }
      }
    }
    // 单类型打包处理
    // _list = _data[dataField[row.productType]].map((v) => {
    //   const { serialNumber, material, packageQuantity, grossWeight, plate, length } = v
    //   return {
    //     serialNumber,
    //     material,
    //     quantity: packageQuantity,
    //     totalWeight: (packageQuantity * grossWeight).toFixed(DP.COM_WT__KG),
    //     // totalNetWeight: totalNetWeight ? totalNetWeight.toFixed(DP.COM_WT__KG) : 0,
    //     plate,
    //     length: length ? length.toFixed(DP.MES_ENCLOSURE_L__MM) : 0,
    //   }
    // })
  } catch (error) {
    console.log('获取详情失败', error)
  }

  // 标签构件信息
  const packageInfo = {
    serialNumber: row.serialNumber,
    list: _list,
    structureList: _structureList,
    auxList: _auxList,
    productType: row.productType,
    project: row.project,
    companyName: printConfig.manufacturerName,
    unitValue: unitValue.value,
    showMaterial: printConfig.showMaterial,
    showWidth: printConfig.showWidth
  }
  // 生产线信息
  return {
    packageInfo,
    qrCode: JSON.stringify({
      id: row.id,
      type: QR_SCAN_TYPE.MES_PACKAGE,
      ftype: QR_SCAN_F_TYPE.MES_PACKAGE_SHIP
    })
  }
}

function handleDateChange() {
  if (query.date && query.date.length > 1) {
    query.startDate = moment(query.date[0]).valueOf()
    query.endDate = moment(query.date[1]).valueOf()
  } else {
    query.startDate = undefined
    query.endDate = undefined
  }
  crud.toQuery()
}

function handleCopiesChange(val) {
  if (!val) {
    printConfig.copies = 1
  }
}

const checkboxMaterial = (v) => {
  printConfig.showMaterial = v
  emit('checkboxMaterial', v)
}

const checkboxWidth = (v) => {
  printConfig.showWidth = v
  emit('checkboxWidth', v)
}

defineExpose({
  print,
  getLabelInfo
})
</script>
