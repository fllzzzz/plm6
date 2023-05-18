<template>
  <div class="hed-container">
    <div v-show="crud.searchToggle">
      <monomer-select-area-tabs
        v-if="productType !== componentTypeEnum.ENCLOSURE.V"
        :project-id="globalProjectId"
        @change="fetchMonomerAndArea"
        :productType="productType"
        needConvert
      />
      <common-radio-button
        v-if="productType === componentTypeEnum.ENCLOSURE.V"
        v-model="query.category"
        :options="planInfo"
        :type="'other'"
        default
        :dataStructure="{ key: 'no', label: 'name', value: 'no' }"
        class="filter-item"
        @change="fetchBatch"
      />
      <area-tabs
        v-if="query.category && productType === componentTypeEnum.ENCLOSURE.V"
        class="filter-item"
        :style="areaInfo.length > 1 ? 'width:calc(100% - 300px)' : 'width:calc(100% - 100px)'"
        v-model="query.planId"
        :area-info="areaInfo"
        :default-tab="defaultTab"
        :show-type="2"
        @tab-click="tabClick"
      />
      <div>
        <product-type-query :productType="productType" :toQuery="crud.toQuery" :query="query" />
        <rrOperation />
      </div>
    </div>
    <production-line-box-select
      ref="plBoxSelectRef"
      v-model="query.productionLineId"
      :selected-able-line-ids="selectedAbleLineIds"
      :selected-able-line-loading="selectedAbleLineLoading"
      :tip="productType !== componentTypeEnum.ENCLOSURE.V ? '* 区域下没有含有任务的生产线' : '* 批次下没有含有任务的生产线'"
      class="filter-item"
      @loaded="handleLinesLoaded"
    />
    <crudOperation>
      <template v-slot:optRight>
        <el-popover v-model:visible="printConfigVisible" placement="bottom-start" width="400">
          <el-form ref="form" :model="printConfig" label-width="90px" size="mini">
            <el-form-item label="重量" v-show="productType & (componentTypeEnum.ARTIFACT.V | componentTypeEnum.MACHINE_PART.V)">
              <common-radio-button v-model="printConfig.weight" :options="printWeightTypeEnum.ENUM" type="enum" />
            </el-form-item>
            <el-form-item label="标签类型">
              <div style="display: flex; align-items: center">
                <common-radio-button
                  v-model="printConfig.type"
                  :unshowVal="productType & componentTypeEnum.ENCLOSURE.V ? [labelTypeEnum.SIMPLE.V] : []"
                  :options="labelTypeEnum.ENUM"
                  type="enum"
                />
                <el-popover placement="right" :title="labelTypeEnum.VL[printConfig.type]" :width="400" trigger="hover">
                  <div style="height: 350px; margin-top: -40px">
                    <span v-html="getMiniLabelHtml({ productType, labelType: printConfig.type, labelData: { printConfig } })"></span>
                  </div>
                  <template #reference>
                    <div style="margin-left: 5px">
                      <common-button size="mini" type="info">
                        <i class="el-icon-view"></i>
                      </common-button>
                    </div>
                  </template>
                </el-popover>
              </div>
            </el-form-item>
            <el-form-item label="显示" v-show="productType & (componentTypeEnum.ARTIFACT.V | componentTypeEnum.MACHINE_PART.V)">
              <span style="display: flex; align-items: center">
                <span style="margin-right: 3px">单体</span><el-checkbox v-model="printConfig.showMonomer" />
                <span style="margin-right: 3px">区域</span><el-checkbox v-model="printConfig.showArea" />
                <span style="margin-right: 3px">生产日期</span><el-checkbox v-model="printConfig.dateInProduced" />
                <!-- <span style="margin-right: 3px">生产线</span><el-checkbox v-model="printConfig.showProductionLine" /> -->
              </span>
            </el-form-item>
            <el-form-item label="显示" v-show="productType & componentTypeEnum.ENCLOSURE.V">
              <span style="display: flex; align-items: center">
                <span style="margin-right: 3px">生产日期</span><el-checkbox v-model="printConfig.dateInProduced" />
              </span>
            </el-form-item>
            <el-form-item label="制造商名称">
              <el-input v-model.trim="printConfig.manufacturerName" maxlength="30" clearable style="width: 250px" />
            </el-form-item>
            <el-form-item label="份数">
              <el-input-number
                v-model="printConfig.copiesQuantity"
                :step="1"
                :min="1"
                :max="99"
                style="width: 250px"
                @change="handleCopiesChange"
              />
            </el-form-item>
          </el-form>
          <template #reference>
            <common-button type="primary" v-permission="permission.printSetting" :disabled="configLoading" size="mini">打印设置</common-button>
          </template>
          <div style="text-align: right">
            <common-button size="small" type="success" @click="cancelConfig">取 消</common-button>
            <common-button :loading="saveLoading" size="small" type="primary" @click="saveConfig">保 存</common-button>
          </div>
        </el-popover>
        <el-tag hit size="medium" style="margin-left: 5px" effect="plain">
          <span v-if="!configLoading">
            <span>标签类型：{{ labelTypeEnum.VL[sourcePrintConfig.type] }}，</span>
            <span
v-show="productType & (componentTypeEnum.ARTIFACT.V | componentTypeEnum.MACHINE_PART.V)"
              >重量：{{ printWeightTypeEnum.VL[sourcePrintConfig.weight] }}，</span
            >
            <span
v-show="productType & (componentTypeEnum.ARTIFACT.V | componentTypeEnum.MACHINE_PART.V)"
              >区域：{{ isShowText(sourcePrintConfig.showArea) }}，</span
            >
            <span
v-show="productType & (componentTypeEnum.ARTIFACT.V | componentTypeEnum.MACHINE_PART.V)"
              >单体：{{ isShowText(sourcePrintConfig.showMonomer) }}，</span
            >
            <span
v-show="productType & (componentTypeEnum.ARTIFACT.V | componentTypeEnum.MACHINE_PART.V | componentTypeEnum.ENCLOSURE.V)"
              >生产日期：{{ isShowText(sourcePrintConfig.dateInProduced) }}，</span
            >
            <span>制造商名称：{{ sourcePrintConfig.manufacturerName || '-' }}，</span>
            <span>份数：{{ sourcePrintConfig.copiesQuantity }}</span>
          </span>
          <i v-else class="el-icon-loading" />
        </el-tag>
      </template>
      <template v-slot:viewLeft>
        <common-button
          v-permission="permission.batchPrint"
          type="success"
          size="mini"
          :disabled="crud.selections.length === 0"
          @click="batchPrint(crud.selections)"
          >批量打印标签
        </common-button>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { getEnclosureLines, getPlanInfo, addEnclosureRecord } from '@/api/mes/label-print/enclosure'
import { getPrintConfig, setPrintConfig } from '@/api/mes/label-print/common'
// import { taskAdd as addPrintRecord } from '@/api/mes/label-print/print-record'
import { getHasTaskLine } from '@/api/mes/common'
import { ref, watch, inject, reactive, defineExpose, computed } from 'vue'
import useUserProjects from '@compos/store/use-user-projects'
import { weightTypeEnum as printWeightTypeEnum } from '@enum-ms/common'
import { TechnologyTypeAllEnum } from '@enum-ms/contract'
import { labelTypeEnum, componentTypeEnum } from '@enum-ms/mes'
import { mapGetters } from '@/store/lib'
import { deepClone } from '@data-type/index'
import { spliceQrCodeUrl, QR_SCAN_PATH } from '@/utils/label'

import usePrintLabel from '@/composables/mes/label-print/use-enclosure-label-print'
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import productTypeQuery from '@comp-mes/header-query/product-type-query'
import monomerSelectAreaTabs from '@comp-base/monomer-select-area-tabs'
import areaTabs from '@/components-system/plan/area-tabs'
import productionLineBoxSelect from '@/components-system/enclosure/production-line-box-select'
import { getMiniLabelHtml } from '@/utils/label/index.js'
import { ElMessage } from 'element-plus'

const areaInfo = ref([])
const defaultTab = ref({})
const typeOption = ref([])
const selectedAbleLineLoading = ref(false)
const selectedAbleLineIds = ref([])
const defaultQuery = {
  serialNumber: '',
  monomerId: { value: undefined, resetAble: false },
  areaId: { value: undefined, resetAble: false },
  productionLineId: { value: undefined, resetAble: false }
}

const { crud, query, CRUD } = regHeader(defaultQuery)

const { globalProjectId, user, requestUrl } = mapGetters(['globalProjectId', 'user', 'requestUrl'])
const { projects } = useUserProjects()
const permission = inject('permission')
const productType = inject('productType')
const printType = inject('printType')

const techOptions = [
  {
    name: '压型彩板',
    no: TechnologyTypeAllEnum.PROFILED_PLATE.V,
    alias: 'ENCLOSURE'
  },
  {
    name: '压型楼承板',
    no: TechnologyTypeAllEnum.PRESSURE_BEARING_PLATE.V,
    alias: 'ENCLOSURE'
  },
  {
    name: '桁架楼承板',
    no: TechnologyTypeAllEnum.TRUSS_FLOOR_PLATE.V,
    alias: 'ENCLOSURE'
  },
  {
    name: '夹芯板',
    no: TechnologyTypeAllEnum.SANDWICH_BOARD.V,
    alias: 'ENCLOSURE'
  },
  {
    name: '折边件',
    no: TechnologyTypeAllEnum.BENDING.V,
    alias: 'ENCLOSURE'
  }
]
// // TODO
// const currentArea = {
//   name: ''
// }

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.printedQuantity = v.printQuantity
    v.printQuantity = 1
    return v
  })
}

const printConfigVisible = ref(false)
const saveLoading = ref(false)
const configLoading = ref(false)
let printConfig = reactive({
  weight: printWeightTypeEnum.NET.V,
  type: labelTypeEnum.COMMON.V,
  showArea: true,
  showMonomer: true,
  dateInProduced: true,
  // showProductionLine: true,
  manufacturerName: '',
  copiesQuantity: 1
})
const sourcePrintConfig = ref(deepClone(printConfig))
const labelTypeVisible = reactive({})
function initLabelTypeVisible() {
  for (const item in labelTypeEnum.V) {
    labelTypeVisible[item] = false
  }
}
initLabelTypeVisible()

function isShowText(bool) {
  return bool ? '显示' : '不显示'
}

watch(
  () => globalProjectId.value,
  (val) => {
    query.category = undefined
    query.planId = undefined
    query.productionLineId = undefined
    typeOption.value = []
    fetchPrintConfig()
    fetchBatch()
    fetchHasTaskLine()
    crud.toQuery()
  }
  // { immediate: true }
)

// 获取打印配置
async function fetchPrintConfig() {
  try {
    configLoading.value = true
    const _data = await getPrintConfig(globalProjectId.value, printType)
    if (_data) {
      const { type, weight, showArea, showMonomer, dateInProduced, manufacturerName, copiesQuantity } = _data
      printConfig.weight = weight
      printConfig.type = type
      // printConfig.showProductionLine = showProductionLine
      printConfig.showArea = showArea
      printConfig.showMonomer = showMonomer
      printConfig.dateInProduced = dateInProduced
      printConfig.manufacturerName = manufacturerName
      printConfig.copiesQuantity = copiesQuantity || 1
      sourcePrintConfig.value = deepClone(printConfig)
    }
  } catch (error) {
    console.log('项目打印配置', error)
  } finally {
    configLoading.value = false
  }
}
// 取消配置恢复数据
function cancelConfig() {
  printConfigVisible.value = false
  printConfig = Object.assign(printConfig, deepClone(sourcePrintConfig.value))
}
// 保存打印配置
async function saveConfig() {
  try {
    saveLoading.value = true
    const config = {
      ...printConfig,
      projectId: globalProjectId.value
    }
    await setPrintConfig({
      ...config,
      printType,
      // showProductionLine: printConfig.showProductionLine,
      showArea: printConfig.showArea,
      showMonomer: printConfig.showMonomer,
      dateInProduced: printConfig.dateInProduced
    })
    printConfigVisible.value = false
    sourcePrintConfig.value = deepClone(printConfig)
    ElMessage({ message: '保存成功', type: 'success' })
  } catch (error) {
    console.log('设置项目打印配置', error)
  } finally {
    saveLoading.value = false
  }
}

function handleCopiesChange(val) {
  if (!val) {
    printConfig.copiesQuantity = 1
  }
}

function fetchMonomerAndArea({ monomerId, areaId }) {
  query.monomerId = monomerId
  query.areaId = areaId
}

const plBoxSelectRef = ref()
const lines = ref([])

async function fetchHasTaskLine() {
  const enclosureIds = []
  query.productionLineId = undefined
  selectedAbleLineIds.value = []
  if (!query.areaId && !query.planId) return
  try {
    selectedAbleLineLoading.value = true
    if (productType !== componentTypeEnum.ENCLOSURE.V) {
      const { ids } = await getHasTaskLine({ areaId: query.areaId, productType: productType })
      if (ids && ids.length > 0) {
        selectedAbleLineIds.value = ids
        query.productionLineId = selectedAbleLineIds.value[0]
      }
    } else {
      const data = await getEnclosureLines({ planId: query.planId, category: query.category })
      for (const item in data) {
        enclosureIds.push(Number(item))
      }
      if (enclosureIds && enclosureIds.length > 0) {
        selectedAbleLineIds.value = enclosureIds
        query.productionLineId = selectedAbleLineIds.value[0]
      }
    }
  } catch (error) {
    console.log('获取有任务的生产线', error)
  } finally {
    selectedAbleLineLoading.value = false
  }
}

function handleLinesLoaded() {
  if (plBoxSelectRef.value) {
    lines.value = plBoxSelectRef.value.getLines()
  }
}

function getLine() {
  let _line = {}
  for (const factory of lines.value) {
    const workshops = factory.workshopList || []
    for (const workshop of workshops) {
      const productionLines = workshop.productionLineList || []
      for (const line of productionLines) {
        if (line.id === query.productionLineId) {
          _line = {
            id: line.id,
            name: line.name,
            factoryId: workshop.factoryId,
            factoryName: workshop.factoryName
          }
          break
        }
      }
    }
  }
  return _line
}

watch(
  [() => query.monomerId, () => query.areaId],
  ([monomerId, areaId]) => {
    fetchHasTaskLine()
    if (monomerId && areaId && query.productionLineId) {
      crud.toQuery()
    }
  },
  { immediate: true }
)

watch(
  () => query.planId,
  (val) => {
    fetchHasTaskLine()
    crud.toQuery()
  }
)

watch(
  () => query.productionLineId,
  (productionLineId) => {
    crud.toQuery()
  },
  { immediate: true }
)

// 获取围护分类
const planInfo = computed(() => {
  const categoryData = []
  const categoryList = projects.value?.find((v) => v.id === globalProjectId.value)?.categorys
  categoryList?.forEach((k) => {
    categoryData.push(k)
  })
  techOptions?.forEach((v) => {
    if (categoryData?.findIndex((o) => o === v.no) > -1) {
      typeOption.value.push(v)
    }
  })
  return typeOption.value
})

// 获取围护批次
async function fetchBatch() {
  if (!query.category) return
  try {
    const data = await getPlanInfo({
      category: query.category,
      projectId: globalProjectId.value
    })
    data?.forEach((v) => {
      v.id = v.planId
      v.name = v.planName
    })
    areaInfo.value = data || []
    if (areaInfo.value.length > 0) {
      defaultTab.value = {
        id: areaInfo.value[0].id + '',
        name: areaInfo.value[0].name
      }
    }
  } catch (e) {
    console.log('获取围护的批次失败', e)
  }
}

function tabClick(val) {
  const { name } = val
  query.planId = name
  crud.toQuery()
}

const { getLabelInfo, printLabelFunc } = inject('headerObj')
const { batchPrint, print } = usePrintLabel({
  getPrintTotalNumber: (row) => (row.printQuantity || 0) * sourcePrintConfig.value.copiesQuantity,
  getLabelInfo: getLabelInfo,
  printFinallyHook: crud.toQuery,
  getLoadingTextFunc: (row) => `${row.name}-${row.serialNumber}`,
  printLabelFunc: printLabelFunc,
  needAddPrintRecord: true,
  addPrintIdField: 'taskId',
  addPrintRecordReq: addEnclosureRecord
})

defineExpose({
  getLine,
  companyName: user.value && user.value.companyName,
  printConfig: sourcePrintConfig,
  spliceQrCodeUrl,
  QR_SCAN_PATH,
  requestUrl,
  print
})
</script>

<style lang="scss" scoped>
.el-form-item {
  align-items: center;
  ::v-deep(.el-form-item__content) {
    line-height: unset;
  }
}
</style>
