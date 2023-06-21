<template>
  <common-table
    ref="tableRef"
    v-bind="$attrs"
    :data="form.steelCoilList"
    :cell-class-name="wrongCellMask"
    :expand-row-keys="expandRowKeys"
    :show-empty-symbol="false"
    return-source-data
    row-key="uid"
    @select="selectTableChange"
    @select-all="selectAllTableChange"
  >
    <el-table-column v-if="!props.boolPartyA && !props.noDetail" type="selection" width="55" align="center" :selectable="selectable" />
    <el-expand-table-column :data="form.steelCoilList" v-model:expand-row-keys="expandRowKeys" row-key="uid" fixed="left">
      <template #default="{ row }">
        <div class="mtb-10" style="margin-left: 30px">
          <el-input
            v-model="row.remark"
            :rows="1"
            :autosize="{ minRows: 1, maxRows: 1 }"
            type="textarea"
            placeholder="备注"
            maxlength="200"
            show-word-limit
            style="width: 400px"
          />
        </div>
        <div v-if="isNotBlank(row.inboundList)" class="flex-rsc mtb-20" style="margin-left: 30px">
          <inbound-info-table :stripe="false" :material="row" :basic-class="basicClass" :list="row.inboundList" style="width: 1600px" />
        </div>
      </template>
    </el-expand-table-column>
    <el-table-column label="序号" type="index" align="center" width="60" fixed="left" />
    <el-table-column prop="serialNumber" label="编号" align="center" min-width="110px" fixed="left" />
    <el-table-column prop="classifyName" label="物料种类" align="center" fixed="left" min-width="120" show-overflow-tooltip>
      <template #default="{ row }">
        <el-tooltip :content="row.classifyParentFullName" :disabled="!row.classifyParentFullName" :show-after="500" placement="top">
          <span v-empty-text="row.classifyName" />
        </el-tooltip>
      </template>
    </el-table-column>
    <el-table-column prop="specification" label="规格" align="center" min-width="160px" fixed="left" show-overflow-tooltip>
      <template #default="{ row }">
        <el-tooltip :content="row.specificationLabels" placement="top">
          <span>{{ row.specification }}</span>
        </el-tooltip>
      </template>
    </el-table-column>
    <el-table-column
      v-if="props.boolPartyA || props.noDetail"
      key="weighingTotalWeight"
      prop="weighingTotalWeight"
      align="center"
      :label="`总重 (${baseUnit.weight.unit})`"
      min-width="135px"
    >
      <template #default="{ row }">
        <common-input-number
          v-model="row.weighingTotalWeight"
          :min="0"
          :max="999999999"
          controls-position="right"
          :controls="false"
          :precision="baseUnit.weight.precision"
          size="mini"
          placeholder="重量"
          @change="handleWeightChange($event, row)"
        />
      </template>
    </el-table-column>
    <template v-if="!props.boolPartyA && !props.noDetail">
      <el-table-column prop="purchaseMete" :label="`采购重量 (${baseUnit.weight.unit})`" align="right" min-width="120px">
        <template #default="{ row }">
          <span>
            <el-tooltip effect="dark" content="已入库量" placement="top">
              <span class="color-green">{{ row.inboundMete }}</span>
            </el-tooltip>
            / {{ row.purchaseMete }}
          </span>
        </template>
      </el-table-column>
      <el-table-column
        key="weighingTotalWeight"
        prop="weighingTotalWeight"
        align="center"
        :label="`实收量 (${baseUnit.weight.unit})`"
        min-width="135px"
      >
        <template #default="{ row }">
          <common-input-number
            v-if="form.selectObj?.[row.mergeId]?.isSelected"
            v-model="row.weighingTotalWeight"
            :min="0"
            :max="999999999"
            controls-position="right"
            :controls="false"
            :precision="baseUnit.weight.precision"
            size="mini"
            placeholder="实收量"
            :class="{ 'over-weight-tip': row.hasOver }"
            @change="handleWeightChange($event, row)"
            @blur="handleOverMete(row)"
          />
          <span v-else>{{ row.weighingTotalWeight || '-' }}</span>
        </template>
      </el-table-column>
    </template>
    <el-table-column prop="thickness" align="center" min-width="100px" :label="`厚 (${baseUnit.thickness.unit})`">
      <template #default="{ row }">
        <common-input-number
          v-if="(props.boolPartyA || props.noDetail) || form.selectObj?.[row.mergeId]?.isSelected"
          v-model="row.thickness"
          :min="0"
          :max="999999"
          controls-position="right"
          :controls="false"
          :precision="baseUnit.thickness.precision"
          size="mini"
          placeholder="厚"
        />
        <span v-else>{{ row.thickness }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="width" align="center" min-width="135px" :label="`宽 (${baseUnit.width.unit})`">
      <template #default="{ row }">
        <common-input-number
          v-if="(props.boolPartyA || props.noDetail) || form.selectObj?.[row.mergeId]?.isSelected"
          v-model="row.width"
          :min="0"
          :max="999999"
          controls-position="right"
          :controls="false"
          :precision="baseUnit.width.precision"
          size="mini"
          placeholder="宽"
        />
        <span v-else>{{ row.width }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="length" align="center" min-width="135px" :label="`长 (${baseUnit.length.unit})`">
      <template #default="{ row }">
        <common-input-number
          v-if="(props.boolPartyA || props.noDetail) || form.selectObj?.[row.mergeId]?.isSelected"
          v-model="row.length"
          :min="0"
          :max="999999999"
          :controls="false"
          :precision="baseUnit.length.precision"
          size="mini"
          placeholder="长"
        />
        <span v-else>{{ row.length }}</span>
      </template>
    </el-table-column>
    <!-- <el-table-column prop="number" align="center" min-width="135px" :label="`数量 (${baseUnit.measure.unit})`">
      <template #default="{ row }">
        <common-input-number
          v-model="row.quantity"
          :max="999999999"
          controls-position="right"
          :controls="false"
          :min="1"
          :step="5"
          :precision="0"
          size="mini"
          placeholder="数量"
        />
      </template>
    </el-table-column> -->
    <el-table-column prop="color" label="颜色" align="center" min-width="140px">
      <template #default="{ row }">
        <el-input
          v-if="(props.boolPartyA || props.noDetail) || ((!props.boolPartyA && !props.noDetail) && form.selectObj?.[row.mergeId]?.isSelected)"
          v-model.trim="row.color"
          maxlength="20"
          size="mini"
          placeholder="颜色"
        />
        <span v-else v-empty-text>{{ row.color }}</span>
      </template>
    </el-table-column>

    <!-- 金额设置 -->
    <price-set-columns v-if="!props.boolPartyA && fillableAmount" />

    <el-table-column prop="brand" label="品牌" align="center" min-width="100px">
      <template #default="{ row }">
        <el-input
          v-if="(props.boolPartyA || props.noDetail) || ((!props.boolPartyA && !props.noDetail) && form.selectObj?.[row.mergeId]?.isSelected)"
          v-model.trim="row.brand"
          maxlength="60"
          size="mini"
          placeholder="品牌"
        />
        <span v-else v-empty-text>{{ row.brand }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="heatNoAndBatchNo" label="卷号" align="center" min-width="150px">
      <template #default="{ row }">
        <el-input
          v-if="(props.boolPartyA || props.noDetail) || ((!props.boolPartyA && !props.noDetail) && form.selectObj?.[row.mergeId]?.isSelected)"
          v-model.trim="row.heatNoAndBatchNo"
          size="mini"
          placeholder="卷号"
          maxlength="200"
        />
        <span v-else v-empty-text>{{ row.heatNoAndBatchNo }}</span>
      </template>
    </el-table-column>

    <el-table-column v-if="props.boolPartyA || props.noDetail" label="操作" width="70" align="center" fixed="right">
      <template #default="{ row, $index }">
        <common-button icon="el-icon-delete" type="danger" size="mini" @click="delRow(row.sn, $index)" />
      </template>
    </el-table-column>
  </common-table>
</template>

<script setup>
import { defineProps, computed, defineExpose, ref, inject, reactive, watch } from 'vue'
import { matClsEnum } from '@/utils/enum/modules/classification'
import { isBlank, isNotBlank, toPrecision } from '@/utils/data-type'
import { DP } from '@/settings/config'

import { regExtra } from '@/composables/form/use-form'
import useTableValidate from '@compos/form/use-table-validate'
import useMatBaseUnit from '@/composables/store/use-mat-base-unit'
import useOverReceive from '@/views/wms/material-inbound/raw-material/application/composables/use-over-receive.js'
import elExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import { createUniqueString } from '@/utils/data-type/string'
import { calcSteelCoilLength } from '@/utils/wms/measurement-calc'
import { positiveNumPattern } from '@/utils/validate/pattern'

import inboundInfoTable from '@/views/wms/material-inbound/raw-material/components/inbound-info-table'
import priceSetColumns from '@/views/wms/material-inbound/raw-material/components/price-set-columns.vue'

const props = defineProps({
  boolPartyA: {
    type: Boolean,
    default: false
  },
  fillableAmount: {
    type: Boolean,
    default: false
  },
  noDetail: {
    type: Boolean,
    default: false
  }
})

const tableRef = ref()
// 当前物料基础类型
const basicClass = matClsEnum.STEEL_COIL.V
const { handleOverMete } = useOverReceive({ meteField: 'weighingTotalWeight' })

const rules = {
  classifyId: [{ required: true, message: '请选择物料种类', trigger: 'change' }],
  width: [
    { required: true, message: '请填写宽度', trigger: 'blur' },
    { pattern: positiveNumPattern, message: '宽度必须大于0', trigger: 'blur' }
  ],
  thickness: [
    { required: true, message: '请填写厚度', trigger: 'blur' },
    { pattern: positiveNumPattern, message: '厚度必须大于0', trigger: 'blur' }
  ],
  weighingTotalWeight: [
    { required: true, message: '请填写重量', trigger: 'blur' },
    { pattern: positiveNumPattern, message: '重量必须大于0', trigger: 'blur' }
  ],
  length: [
    { required: true, message: '请填写长度', trigger: 'blur' },
    { pattern: positiveNumPattern, message: '长度必须大于0', trigger: 'blur' }
  ]
  // quantity: [{ required: true, message: '请填写数量', trigger: 'blur' }]
}

// 金额校验
const validateAmount = (value, row) => {
  if (isNotBlank(row.weighingTotalWeight) && isNotBlank(row.unitPrice)) {
    return +toPrecision(row.weighingTotalWeight * row.unitPrice, DP.YUAN) === +value
  }
  return false
}

// 甲供不需要填写价格
const amountRules = {
  unitPrice: [{ required: true, message: '请填写单价', trigger: 'blur' }],
  amount: [
    { required: true, message: '请填写金额', trigger: 'blur' },
    { validator: validateAmount, message: '金额有误，请手动修改', trigger: 'blur' }
  ]
}

const tableRules = computed(() => {
  let _rules = Object.assign({}, rules)
  if (!props.boolPartyA && props.fillableAmount) {
    _rules = Object.assign(_rules, amountRules)
  }
  return _rules
})

const matSpecRef = inject('matSpecRef') // 调用父组件matSpecRef
const { baseUnit } = useMatBaseUnit(basicClass) // 当前分类基础单位
const { form } = regExtra() // 表单
const expandRowKeys = ref([]) // 展开行key

const { tableValidate, wrongCellMask } = useTableValidate({ rules: tableRules, errorMsg: '请修正【钢卷清单】中标红的信息' }) // 表格校验

function selectable(row, rowIndex) {
  return !!row.canPurchaseQuantity || true
}

function selectTableChange(select, row) {
  const boolSelect = Boolean(select.findIndex((v) => v.id === row.id) !== -1)
  form.selectObj[row.mergeId].isSelected = boolSelect
}

function selectAllTableChange(select) {
  const boolSelect = Boolean(select?.length)
  form.steelCoilList.forEach((v) => {
    form.selectObj[v.mergeId].isSelected = boolSelect
  })
}

// 设置选择的回显
function setSelect() {
  form.steelCoilList.forEach((v) => {
    if (form.selectObj?.[v.mergeId]?.isSelected) {
      tableRef.value.toggleRowSelection(v, true)
    }
  })
}

// 行初始化
function rowInit(row) {
  const _row = reactive({
    uid: createUniqueString(),
    sn: row.sn, // 该科目规格唯一编号
    specificationLabels: row.specificationLabels, // 规格中文
    serialNumber: row.serialNumber, // 科目编号 - 规格
    classifyId: row.classify.id, // 科目id
    classifyFullName: row.classify.fullName, // 全路径名称
    classifyName: row.classify.name, // 当前科目名称
    classifyParentFullName: row.classify.parentFullName, // 父级路径名称
    basicClass: row.classify.basicClass, // 基础类型
    specification: row.spec, // 规格
    specificationMap: row.specKV, // 规格KV格式
    measureUnit: row.classify.measureUnit, // 计量单位
    accountingUnit: row.classify.accountingUnit, // 核算单位
    accountingPrecision: row.classify.accountingPrecision, // 核算单位小数精度
    measurePrecision: row.classify.measurePrecision, // 计量单位小数精度
    quantity: undefined, // 数量（毫米，计量单位对应的值）
    color: undefined, // 颜色
    brand: undefined, // 品牌
    heatNoAndBatchNo: undefined, // 炉批号
    thickness: undefined, // 厚度
    length: undefined, // 长度
    width: undefined, // 宽度
    theoryLength: undefined, // 理论单件重量
    weighingTotalWeight: undefined // 过磅重量
  })

  // 非甲供
  if (!props.boolPartyA && props.fillableAmount) {
    _row.unitPrice = undefined // 含税单价
    _row.amount = undefined // 金额
  }
  rowWatch(_row)
  return _row
}

// 行监听
// 使用watch 监听方法，优点：初始化时表单数据时，可以不立即执行（惰性），可以避免“草稿/修改”状态下重量被自动修改；缺点：初始化时需要指定监听参数
function rowWatch(row) {
  // watchEffect(() => calcTheoryLength(_row))
  // watchEffect(() => calcTotalLength(_row))
  watch(
    () => row,
    () => {
      if ((!props.boolPartyA && !props.noDetail) && form.selectObj?.[row.mergeId]?.isSelected) {
        const _isSelected = form.selectObj[row.mergeId]?.isSelected
        form.selectObj[row.mergeId] = {
          ...form.selectObj[row.mergeId],
          ...row,
          mete: row.weighingTotalWeight,
          isSelected: _isSelected
        }
      }
    },
    { deep: true }
  )
  // 计算理论长度
  watch([() => row.weighingTotalWeight, () => row.width, () => row.thickness, baseUnit], () => calcTheoryLength(row))
  // 计算总长度
  watch([() => row.theoryLength], () => calcTotalLength(row))
}

// 总重计算与单位重量计算分开，避免修改数量时需要重新计算单件重量
// 计算单件重量
async function calcTheoryLength(row) {
  row.theoryLength = await calcSteelCoilLength({
    name: row.classifyFullName,
    weight: row.weighingTotalWeight,
    width: row.width,
    thickness: row.thickness
  })
}

// 计算总长(没有数量概念，可以直接和calcTheoryLength合并)
function calcTotalLength(row) {
  if (isNotBlank(row.theoryLength)) {
    row.length = row.theoryLength * 1
  } else {
    row.length = undefined
  }
  // row.quantity = row.length
}

// 处理重量变化
function handleWeightChange(val, row) {
  if (isNotBlank(row.unitPrice) && isNotBlank(val)) {
    row.amount = toPrecision(val * row.unitPrice, DP.YUAN)
  }
}

// 删除行
function delRow(sn, $index) {
  if (matSpecRef.value) {
    matSpecRef.value.delListItem(sn, $index)
  } else {
    form.steelCoilList.splice($index, 1)
  }
}

// 校验
function validate() {
  const _list = form.steelCoilList.filter((v) => {
    if ((props.boolPartyA || props.noDetail) || form.selectObj[v.mergeId]?.isSelected) {
      return true
    } else {
      return false
    }
  })
  if (isBlank(_list)) return true
  const { validResult } = tableValidate(_list)
  return validResult
}

function toggleRowSelection(row, selected) {
  tableRef?.value?.toggleRowSelection(row, selected)
}

defineExpose({
  rowInit,
  rowWatch,
  toggleRowSelection,
  validate,
  setSelect
})
</script>
