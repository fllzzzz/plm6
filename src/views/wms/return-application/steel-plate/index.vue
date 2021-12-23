<template>
  <div class="app-container">
    <common-header ref="headerRef" :basic-class="basicClass" :list="form.list" :current-source="currentSource" @add="rowWatch" />
    <el-form ref="formRef" :model="form" :disabled="cu.status.edit === FORM.STATUS.PROCESSING">
      <common-table
        ref="tableRef"
        :data="form.list"
        :max-height="maxHeight"
        :default-expand-all="false"
        :stripe="false"
        :cell-class-name="wrongCellMask"
        highlight-current-row
        row-key="uid"
        @row-click="handleRowClick"
      >
        <el-table-column label="序号" type="index" align="center" width="60" fixed="left">
          <template #default="{ row, $index }">
            <div v-if="row.overTipColor" class="left-triangle-tip" :style="{ 'border-left-color': row.overTipColor }" />
            <span>{{ $index + 1 }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="source.project" label="项目" align="left" min-width="120px" fixed="left" show-overflow-tooltip>
          <template #default="{ row }">
            <span v-parse-project="{ project: row.source.project, onlyShortName: true }" v-empty-text />
          </template>
        </el-table-column>
        <el-table-column prop="source.serialNumber" label="编号" align="center" width="110px" fixed="left" />
        <el-table-column prop="source.classifyFullName" label="物料种类" align="center" width="120px" fixed="left" />
        <el-table-column prop="source.specification" label="规格" align="center" width="100px" fixed="left">
          <template #default="{ row }">
            <el-tooltip :content="row.source.specificationLabels" placement="top">
              <span v-empty-text>{{ row.source.specification }}</span>
            </el-tooltip>
          </template>
        </el-table-column>
        <!-- 次要信息 -->
        <material-secondary-info-columns :basic-class="basicClass" field="source" fixed="left" />
        <el-table-column prop="source.thickness" align="center" width="100px" :label="`厚 (${baseUnit.thickness.unit})`" fixed="left">
          <template #default="{ row }">
            <span v-to-fixed="baseUnit.thickness.precision">{{ row.source.thickness }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="width" align="center" width="110px" :label="`宽 (${baseUnit.width.unit})`">
          <template #default="{ row }">
            <el-input-number
              v-model="row.width"
              :min="0"
              :max="+row.source.width"
              controls-position="right"
              :controls="false"
              :precision="baseUnit.width.precision"
              size="mini"
              placeholder="宽"
            />
          </template>
        </el-table-column>
        <el-table-column prop="length" align="center" width="110px" :label="`长 (${baseUnit.length.unit})`">
          <template #default="{ row }">
            <el-input-number
              v-model="row.length"
              :max="+row.source.length"
              :controls="false"
              :min="0"
              :precision="baseUnit.length.precision"
              size="mini"
              placeholder="长"
            />
          </template>
        </el-table-column>
        <el-table-column prop="quantity" align="center" width="110px" :label="`数量 (${baseUnit.measure.unit})`">
          <template #default="{ row }">
            <el-input-number
              v-model="row.quantity"
              :min="1"
              :max="+row.source.quantity"
              controls-position="right"
              :controls="false"
              :step="1"
              :precision="baseUnit.measure.precision"
              size="mini"
              placeholder="数量"
            />
          </template>
        </el-table-column>
        <el-table-column key="mete" prop="mete" align="center" :label="`总重 (${baseUnit.weight.unit})`" width="120px">
          <template #default="{ row }">
            <el-input-number
              v-model="row.mete"
              :min="0"
              :max="+row.maxTotalWeight"
              controls-position="right"
              :controls="false"
              :precision="baseUnit.weight.precision"
              size="mini"
              placeholder="重量"
            />
          </template>
        </el-table-column>
        <!-- 仓库设置 -->
        <warehouse-set-columns :list="form.list" />
        <el-table-column label="操作" width="70" align="center" fixed="right">
          <template #default="{ row, $index }">
            <common-button icon="el-icon-delete" type="danger" size="mini" @click="delRow(row, $index)" />
          </template>
        </el-table-column>
      </common-table>
    </el-form>
  </div>
</template>

<script setup>
import { steelPlateReturnApplication } from '@/api/wms/return/application'
import { edit as editReturnApplication } from '@/api/wms/return/raw-mat-application-record'

import { ref, watch, defineEmits, defineProps, reactive, nextTick } from 'vue'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import { calcSteelPlateWeight } from '@/utils/wms/measurement-calc'
import { isNotBlank, toFixed } from '@/utils/data-type'

import useMaxHeight from '@compos/use-max-height'
import useMatBaseUnit from '@/composables/store/use-mat-base-unit'
import useTableValidate from '@/composables/form/use-table-validate'
import useForm from '@/composables/form/use-form'
import MaterialSecondaryInfoColumns from '@/components-system/wms/table-custom-field-columns/material-secondary-info-columns/index.vue'
import WarehouseSetColumns from '../components/warehouse-set-columns.vue'
import CommonHeader from '../components/common-header.vue'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { ElMessage } from 'element-plus'
import { getLightColor } from '@/utils/color'

const emit = defineEmits(['success'])

const props = defineProps({
  edit: {
    type: Boolean,
    default: false
  },
  detail: {
    type: Object
  }
})

// 权限
const permission = ['wms_steelReturnApplication:submit']
// 默认表单
const defaultForm = {
  list: []
}

const headerRef = ref()
const tableRef = ref()
const formRef = ref()
// 最大高度
const { maxHeight } = useMaxHeight({ paginate: false })
// 钢板类型
const basicClass = rawMatClsEnum.STEEL_PLATE.V
// 当前分类基础单位
const { baseUnit } = useMatBaseUnit(basicClass)
// 当前退库源数据
const currentSource = ref()
// 当前高亮uid
const currentUid = ref()

const tableRules = {
  id: [{ required: true, message: '请选择退库物料', trigger: 'change' }],
  width: [{ required: true, message: '请填写宽度', trigger: 'blur' }],
  length: [{ required: true, message: '请填写长度', trigger: 'blur' }],
  mete: [{ required: true, message: '请填写重量', trigger: 'blur' }],
  quantity: [{ required: true, message: '请填写数量', trigger: 'blur' }],
  factoryId: [{ required: true, message: '请选择工厂', trigger: 'change' }],
  warehouseId: [{ required: true, message: '请选择存储位置', trigger: 'change' }]
}

// 同上数据
const ditto = new Map([
  ['factoryId', -1],
  ['warehouseId', -1]
])

// 使用草稿/修改时，为数据设置监听
const setFormCallback = (form) => {
  form.list = form.list.map((v) => reactive(v))
  const trigger = watch(
    tableRef,
    (ref) => {
      if (ref) {
        // 将相同的材料设置为同一个对象，便于计算
        extractSource(form)
        // 初始化选中数据，执行一次后取消当前监听
        form.list.forEach((v) => rowWatch(v))
        nextTick(() => {
          trigger()
        })
      }
    },
    { immediate: true, deep: true }
  )
}

const { cu, form, FORM } = useForm(
  {
    title: '钢材退库',
    formStore: !props.edit,
    formStoreKey: 'WMS_RETURN_APPLICATION_STEEL_PLATE',
    permission: permission,
    defaultForm: defaultForm,
    useDraftCallback: setFormCallback,
    clearDraftCallback: init,
    api: props.edit ? editReturnApplication : steelPlateReturnApplication
  },
  formRef,
  props.detail
)

const { tableValidate, wrongCellMask, cleanUpData } = useTableValidate({
  rules: tableRules,
  ditto,
  errorMsg: '请修正【退库清单】中标红的信息'
}) // 表格校验

// 监听list
watch(
  () => form.list,
  () => {
    setDefaultCurrent()
  },
  { deep: true }
)

// 表单提交数据清理
cu.submitFormFormat = async (form) => {
  cleanUpData(form.list)
  form.list = await numFmtByBasicClass(form.list, { toSmallest: true, toNum: true })
  return form
}

FORM.HOOK.beforeToEdit = async (crud, form) => {
  if (!props.edit) return
  // 修改的情况下，数据预处理
  // await steelInboundFormFormat(form)
  // 设置监听等
  setFormCallback(form)
}

// 校验
FORM.HOOK.beforeValidateCU = (crud, form) => {
  return validate()
}

// 提交后清除校验结果
FORM.HOOK.afterSubmit = () => {
  if (props.edit) {
    emit('success')
  }
  init()
}

// 初始化
function init() {
  form.list = []
  // 当前数据
  currentSource.value = undefined
  // 当前高亮uid
  currentUid.value = undefined
}

// 校验
function validate() {
  const { validResult, dealList } = tableValidate(form.list)
  form.list = dealList
  if (validResult) {
    return validateOverSource()
  }
  return false
}

// 校验是否超过原材料可还库最大值
function validateOverSource() {
  const hasOver = form.list.some((v) => v.overTipColor)
  if (hasOver) {
    ElMessage.error('请修正序号前带“三角形标志”的数据，他们的合计重量，超过了可还库的总重')
  }
  return !hasOver
}

// 行选中
function handleRowClick(row, column, event) {
  currentUid.value = row.uid
  currentSource.value = row.source
}

// 删除当前行
function delRow(row, index) {
  form.list.splice(index, 1)
  // 删除后计算是否超出
  checkOverSource(row)
  if (row.uid === currentUid.value || !currentUid.value) {
    setTimeout(() => {
      if (form.list.length > 0) {
        // delRow后会触发handleRowClick,因此延迟触发
        const newCurrent = index < form.list.length ? form.list[index] : form.list[index - 1]
        tableRef.value.setCurrentRow(newCurrent)
        handleRowClick(newCurrent)
      } else {
        // 数据为空时，选中设置为null
        currentUid.value = null
        currentSource.value = null
      }
    }, 0)
  }
}

// 设置默认选中行
function setDefaultCurrent() {
  if (form.list.length > 0 && !currentUid.value) {
    // 选中第一行
    const newCurrent = form.list[0]
    tableRef.value.setCurrentRow(newCurrent)
    handleRowClick(newCurrent)
  }
}

// 添加材质
function rowWatch(row) {
  // 计算最大总重
  watch([() => row.quantity], () => {
    calcMaxTotalWeight(row)
    headerRef.value && headerRef.value.calcAllQuantity()
  })
  // 计算理论及单重
  watch([() => row.length, () => row.width, baseUnit], () => calcTheoryWeight(row))
  // 计算总重
  watch([() => row.singleMete, () => row.quantity], () => {
    calcTotalWeight(row)
    headerRef.value && headerRef.value.calcAllWeight()
  })
  watch(
    () => row.mete,
    () => {
      checkOverSource(row)
    }
  )
}

// 计算单件理论重量
async function calcTheoryWeight(row) {
  row.theoryWeight = await calcSteelPlateWeight({
    name: row.source.classifyFullName, // 名称，用于判断是否为不锈钢，不锈钢与普通钢板密度不同
    length: row.length,
    width: row.width,
    thickness: row.source.thickness
  })
  if (row.theoryWeight) {
    row.singleMete = +toFixed((row.theoryWeight / row.source.theoryWeight) * row.source.singleReturnableMete)
  } else {
    row.singleMete = undefined
  }
}

// 计算总重
function calcTotalWeight(row) {
  if (isNotBlank(row.singleMete) && row.quantity) {
    row.mete = +toFixed(row.singleMete * row.quantity, baseUnit.value.weight.precision)
  } else {
    row.mete = undefined
  }
}

// 计算最大重量
function calcMaxTotalWeight(row) {
  if (row.quantity) {
    row.maxTotalWeight = row.source.singleReturnableMete * row.quantity
  } else {
    row.maxTotalWeight = row.source.singleReturnableMete
  }
}

// 提取退库材料相同的对象
function extractSource(form) {
  const sourceKV = {}
  form.list.forEach(v => {
    if (sourceKV[v.source.id]) {
      v.source = sourceKV[v.source.id]
    } else {
      sourceKV[v.source.id] = v.source
    }
  })
}

// 计算退库信息
function calcReturnInfo() {
  const mete = {}
  const sourceKV = {}
  form.list.forEach((v) => {
    if (!sourceKV[v.source.id]) {
      sourceKV[v.source.id] = v.source
    }
    if (isNotBlank(mete[v.id])) {
      mete[v.id] += v.mete || 0
    } else {
      mete[v.id] = v.mete || 0
    }
  })
  Object.keys(sourceKV).forEach(key => {
    const sourceMaterial = sourceKV[key]
    sourceMaterial.returnableMete = sourceMaterial.sourceReturnableMete - (mete[sourceMaterial.id] || 0)
  })
}

// 校验是否超出原材料的可退库量
function checkOverSource(row) {
  calcReturnInfo()
  if (row.source.returnableMete < 0 && !row.overTipColor) {
    const overTipColor = getLightColor()
    row.overTipColor = overTipColor
    form.list.forEach((r) => {
      if (r.id === row.id) {
        r.overTipColor = overTipColor
      }
    })
  }
  if (row.source.returnableMete >= 0 && row.overTipColor) {
    form.list.forEach((r) => {
      if (r.id === row.id) {
        r.overTipColor = undefined
      }
    })
  }
}
</script>

<style lang="scss" scoped>
.el-table {
  ::v-deep(.current-row > td.el-table__cell) {
    --el-table-current-row-background-color: #d7ffef;
  }
}
</style>
