<template>
  <div class="app-container">
    <common-header
      ref="headerRef"
      :edit="props.edit"
      :basic-class="basicClass"
      :list="form.list"
      :current-source="currentSource"
      :currentPlateRow="currentPlateRow"
      :source-return-ids="sourceReturnIds"
      @submit="formSubmit"
      @add="rowWatch"
    />
    <el-form ref="formRef" :model="form" :disabled="cu.status.edit === FORM.STATUS.PROCESSING">
      <common-table
        ref="tableRef"
        :data="form.list"
        :data-format="columnsDataFormat"
        :max-height="maxHeight"
        :default-expand-all="false"
        :stripe="false"
        :cell-class-name="wrongCellMask"
        :expand-row-keys="expandRowKeys"
        row-key="uid"
        highlight-current-row
        @row-click="handlePlateRowClick"
      >
        <el-expand-table-column :data="form.list" v-model:expand-row-keys="expandRowKeys" :stripe="false" row-key="uid" style="background:#d7ffef;">
          <template #default="{ row: { sourceRow: row } }">
            <div class="mtb-10" :style="`padding-right:20px;padding-left:20px;background:${errorKey.includes(row.uid)?'#ff000021':(currentUid===row.uid?'#d7ffef':'')};`">
              <common-table :class="errorKey.includes(row.uid)?'error-table':(currentUid===row.uid?'child-table':'')" :key="row.uid" :data="row.list" style="margin-bottom:10px;" :stripe="false" v-if="row.boolReturns && (row.uid || row.id)">
                <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
                 <material-base-info-columns
                  :key="row.uid"
                  :basic-class="basicClass"
                  field="source"
                  :show-project="false"
                  :show-index="false"
                  party-a-position="project"
                  :show-width="false"
                  :show-length="false"
                  :show-thickness="false"
                />
                <material-secondary-info-columns :basic-class="basicClass" field="source" fixed=" "/>
                <el-table-column prop="source.thickness" align="center" width="70px" :label="`厚 (${baseUnit.thickness.unit})`" />
                <el-table-column prop="width" align="center" width="110px" :label="`宽 (${baseUnit.width.unit})`">
                  <template #default="{ row: { sourceRow: row } }">
                    <common-input-number
                      v-model="row.width"
                      :min="(steelMinLengthConfig?.steelPlateShortestSideMinLength+1) || 0"
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
                  <template #default="{ row: { sourceRow: row } }">
                    <common-input-number
                      v-model="row.length"
                      :max="+row.source.length"
                      :controls="false"
                      :min="(steelMinLengthConfig?.steelPlateShortestSideMinLength+1) || 0"
                      :precision="baseUnit.length.precision"
                      size="mini"
                      placeholder="长"
                      @blur="checkLength(row)"
                    />
                  </template>
                </el-table-column>
                <el-table-column prop="quantity" align="center" width="110px" :label="`数量 (${baseUnit.measure.unit})`">
                  <template #default="{ row: { sourceRow: row } }">
                    <common-input-number
                      v-model="row.quantity"
                      :min="1"
                      :max="9999999"
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
                  <template #default="{ row: { sourceRow: row } }">
                    <common-input-number
                      v-model="row.mete"
                      :min="0"
                      :max="9999999"
                      controls-position="right"
                      :controls="false"
                      :precision="baseUnit.weight.precision"
                      size="mini"
                      placeholder="重量"
                    />
                  </template>
                </el-table-column>
                <warehouse-set-columns :list="row.list" />
                <el-table-column label="操作" width="70" align="center">
                  <template #default="{ row: { sourceRow: row }, $index }">
                    <common-button icon="el-icon-delete" type="danger" size="mini" @click="delChildRow(row, $index)" />
                  </template>
                </el-table-column>
              </common-table>
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
          </template>
        </el-expand-table-column>
        <!-- 基础信息 -->
        <material-base-info-columns
          :basic-class="basicClass"
          field="source"
          showTip
          show-project
          party-a-position="project"
          :show-width="false"
          :show-length="false"
          :show-thickness="false"
          :key="basicClass"
        />
        <!-- 次要信息 -->
        <material-secondary-info-columns :basic-class="basicClass" field="source" fixed=" " />
        <el-table-column prop="source.thickness" align="center" width="70px" :label="`厚 (${baseUnit.thickness.unit})`" />
        <el-table-column prop="width" align="center" width="110px" :label="`宽 (${baseUnit.width.unit})`">
          <template #default="{ row: { sourceRow: row } }">
            <!-- <common-input-number
              v-if="!row.boolReturns"
              v-model="row.width"
              :min="0"
              :max="+row.source.width"
              controls-position="right"
              :controls="false"
              :precision="baseUnit.width.precision"
              size="mini"
              placeholder="宽"
            />
            <span v-else>{{row.width}}</span> -->
            <span>{{row.width}}</span>
          </template>
        </el-table-column>
        <el-table-column prop="length" align="center" width="110px" :label="`长 (${baseUnit.length.unit})`">
          <template #default="{ row: { sourceRow: row } }">
            <!-- <common-input-number
              v-if="!row.boolReturns"
              v-model="row.length"
              :max="+row.source.length"
              :controls="false"
              :min="steelMinLengthConfig?.steelPlateShortestSideMinLength || 0"
              :precision="baseUnit.length.precision"
              size="mini"
              :disabled="!loaded"
              placeholder="长"
            />
            <span v-else>{{row.length}}</span> -->
            <span>{{row.length}}</span>
          </template>
        </el-table-column>
        <el-table-column prop="quantity" align="center" width="110px" :label="`数量 (${baseUnit.measure.unit})`">
          <template #default="{ row: { sourceRow: row } }">
            <common-input-number
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
        <el-table-column key="detailMete" prop="detailMete" align="center" :label="`余料(${baseUnit.weight.unit})`" width="120px">
           <template #default="{ row: { sourceRow: row } }">
            <span>{{ row.boolReturns?row.detailMete || '-':'-'}}</span>
          </template>
        </el-table-column>
        <el-table-column key="mete" prop="mete" align="center" :label="`总重 (${baseUnit.weight.unit})`" width="120px">
          <template #default="{ row: { sourceRow: row } }">
            <!-- <common-input-number
              v-if="!row.boolReturns"
              v-model="row.mete"
              :min="0"
              :max="+(row.singleMete * row.quantity)"
              controls-position="right"
              :controls="false"
              :precision="baseUnit.weight.precision"
              size="mini"
              placeholder="重量"
            />
            <span v-else>{{ row.boolReturns?(row.detailMete || '-') + ' | ' +row.mete:row.mete}}</span> -->
            <span>{{ row.mete }}</span>
          </template>
        </el-table-column>
        <!-- 仓库设置 -->
        <warehouse-set-columns :list="form.list" />
        <el-table-column label="操作" width="120" align="center" fixed="right">
          <template #default="{ row: { sourceRow: row }, $index }">
            <common-button icon="el-icon-plus" type="primary" size="mini" @click="addChildren(row)" v-if="row.boolReturns"/>
            <common-button icon="el-icon-delete" type="danger" size="mini" @click="delRow(row, $index)" />
          </template>
        </el-table-column>
      </common-table>
    </el-form>
  </div>
</template>

<script setup>
import { steelPlateReturnApplication } from '@/api/wms/material-return/raw-material/application'
import { edit as editReturnApplication } from '@/api/wms/material-return/raw-material/record'
import { steelPlateReturnApplicationPM as permission } from '@/page-permission/wms'

import { ref, watch, defineEmits, defineProps, reactive, nextTick } from 'vue'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import { calcSteelPlateWeight } from '@/utils/wms/measurement-calc'
import { isNotBlank, toPrecision } from '@/utils/data-type'
import { positiveNumPattern } from '@/utils/validate/pattern'
import { createUniqueString } from '@/utils/data-type/string'
import { ElMessage } from 'element-plus'

import useMaxHeight from '@compos/use-max-height'
import useMatBaseUnit from '@/composables/store/use-mat-base-unit'
import useForm from '@/composables/form/use-form'
import MaterialBaseInfoColumns from '@/components-system/wms/table-custom-field-columns/material-base-info-columns/index.vue'
import MaterialSecondaryInfoColumns from '@/components-system/wms/table-custom-field-columns/material-secondary-info-columns/index.vue'
import elExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import WarehouseSetColumns from '../components/warehouse-set-columns.vue'
import CommonHeader from '../components/common-header.vue'
import useCurrentRow from '../composables/use-current-row'
import useFormSet from '../composables/use-form-set'
import useCommonCalc from '../composables/use-common-calc'
// 废料定义，退库长度应大于废料
import useSteelMinLengthConfig from '@compos/store/use-steel-minlength-config'

const { steelMinLengthConfig } = useSteelMinLengthConfig()

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

// 默认表单
const defaultForm = {
  list: []
}

const expandRowKeys = ref([]) // 展开行key
const headerRef = ref()
const tableRef = ref()
const formRef = ref()
const errorKey = ref([])
const currentPlateRow = ref({})
// 表格列格式化
const columnsDataFormat = ref([['source.project', ['parse-project', { onlyShortName: true }]]])
// 最大高度
const { fixMaxHeight, maxHeight } = useMaxHeight({ paginate: false })
// 钢板类型
const basicClass = rawMatClsEnum.STEEL_PLATE.V
// 当前分类基础单位
const { baseUnit } = useMatBaseUnit(basicClass)

// 重量
const validateMete = (value, row) => {
  if (!value) return false
  if (row.boolReturns) {
    if (!row.detailMete || row.detailMete > value) {
      return false
    }
    return true
  }
  return true
}

const validateLength = (value, row) => {
  if (!value) return false
  if (steelMinLengthConfig.value.steelPlateShortestSideMinLength && value <= steelMinLengthConfig.value.steelPlateShortestSideMinLength) {
    return false
  }
  return true
}

const tableRules = {
  width: [
    { required: true, message: '请填写宽度', trigger: 'blur' },
    { pattern: positiveNumPattern, message: '宽度必须大于0', trigger: 'blur' }
  ],
  length: [
    { required: true, message: '请填写长度', trigger: 'blur' },
    { validator: validateLength, message: '长度必须大于废料长度', trigger: 'blur' }
  ],
  mete: [
    { validator: validateMete, message: '请填写重量', trigger: 'blur' }
  ],
  quantity: [
    { required: true, message: '请填写数量', trigger: 'blur' },
    { pattern: positiveNumPattern, message: '数量必须大于0', trigger: 'blur' }
  ],
  factoryId: [{ required: true, message: '请选择工厂', trigger: 'change' }],
  warehouseId: [{ required: true, message: '请选择存储位置', trigger: 'change' }]
}

const { cu, form, FORM } = useForm(
  {
    title: '钢板退库',
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
// 设置 boolRealReturn 状态
cu.updateProp('boolRealReturn', props.detail && Object.prototype.hasOwnProperty.call(props.detail, 'boolRealReturn') ? props.detail.boolRealReturn : true)

// 通用计算校验
const { calcMaxMete, extractSource, checkOverSource, initCheckOverMaxWeight } = useCommonCalc({ cu, form, basicClass })

// 高亮行处理
const { currentSource, currentUid, delRow, handleRowClick } = useCurrentRow({ form, tableRef, delCallback: checkOverSource })
// 表单信息及校验
const { sourceReturnIds, wrongCellMask } = useFormSet({
  FORM,
  form,
  cu,
  emit,
  tableRules,
  init,
  setFormCallback,
  isEdit: props.edit
})

watch(
  () => form.list,
  () => {
    headerRef.value && headerRef.value.calcAllQuantity()
    headerRef.value && headerRef.value.calcAllWeight()
  },
  { deep: true }
)

function handlePlateRowClick(val) {
  currentPlateRow.value = val
  handleRowClick(val)
}

// 初始化
function init() {
  form.list = []
  // 当前数据
  currentSource.value = undefined
  // 当前高亮uid
  currentUid.value = undefined
  currentPlateRow.value = {}
  errorKey.value = []
  // 异常列表
  // cu.props.abnormalList = undefined
}

// 添加材质
function rowWatch(row) {
  // 计算最大总重
  watch([() => row.quantity], () => {
    if (!row.boolReturns) {
      calcMaxMete(row)
    }
    headerRef.value && headerRef.value.calcAllQuantity()
  },
  { immediate: true })
  // 计算理论及单重
  watch([() => row.length, () => row.width, baseUnit], () => {
    calcTheoryWeight(row)
  },
  { immediate: true })
  // 计算总重
  watch([() => row.singleMete, () => row.quantity], () => {
    calcTotalWeight(row)
    headerRef.value && headerRef.value.calcAllWeight()
  },
  { immediate: true })
  watch(
    () => row.mete,
    () => {
      checkOverSource(row)
    },
    { immediate: true }
  )
  setRow(row, row.source)
  const filterVal = form.list.filter(v => v.boolReturns) || []
  const filterArr = filterVal.map(v => v.uid) || []
  expandRowKeys.value = filterArr
}
function childrenWatch(row) {
  // 计算理论及单重
  watch([() => row.length, () => row.width, baseUnit], () => {
    calcTheoryWeight(row)
  },
  { immediate: true })
  // 计算总重
  watch([() => row.singleMete, () => row.quantity], () => {
    calcTotalWeight(row)
  },
  { immediate: true })
  // 计算表格总重
  watch([() => row.mete], () => {
    calcTableTotalWeight(row)
  },
  { immediate: true })
}

// 设置行默认值
function setRow(row, sourceRow) {
  row.width = row.width || sourceRow.width
  row.length = row.length || sourceRow.length
  row.quantity = row.quantity || sourceRow.quantity
  row.factoryId = row.factoryId || sourceRow.factory?.id
  row.warehouseId = row.warehouseId || sourceRow.warehouse?.id
  calcTheoryWeight(row)
  setTimeout(() => {
    row.mete = row.mete || sourceRow.mete
  })
}

function addChildren(row) {
  const val = JSON.parse(JSON.stringify(row))
  delete val.list
  if (row.list.length > 1) {
    val.factoryId = -1 // 工厂 同上
    val.warehouseId = -1 // 仓库 同上
  }
  const pushVal = reactive({
    ...val,
    pid: val.uid,
    quantity: 1,
    mete: val.singleMete,
    uid: createUniqueString()
  })
  row.list.push(pushVal)
  childrenWatch(pushVal)
}

function delChildRow(row, index) {
  const findVal = form.list.find(v => v.uid === row.pid) || {}
  if (isNotBlank(findVal.list)) {
    findVal.detailMete = toPrecision(findVal.detailMete - row.mete, baseUnit.value.weight.precision)
    findVal.list.splice(index, 1)
  }
}

function checkLength(row) {
  if (row.length && row.length <= steelMinLengthConfig.value.steelPlateShortestSideMinLength) {
    row.length = undefined
  }
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
    // 将小数精度提高一位计算，计算总重与实际总重出现误差
    row.singleMete = toPrecision((row.theoryWeight / row.source.theoryWeight) * row.source.singleMete, 5)
  } else {
    row.singleMete = undefined
  }
}

function calcTableTotalWeight(row) {
  const findVal = form.list.find(v => v.uid === row.pid) || {}
  if (isNotBlank(findVal.list)) {
    let detailMete = 0
    findVal.list.forEach(v => {
      if (v.mete) {
        detailMete += v.mete
      }
    })
    findVal.detailMete = toPrecision(detailMete, baseUnit.value.weight.precision)
  }
}

// 计算总重
function calcTotalWeight(row) {
  if (isNotBlank(row.singleMete) && row.quantity) {
    // row.mete = toPrecision(
    //   row.maxMete * (row.singleMete / row.source.singleMete) * (row.quantity / row.maxQuantity),
    //   baseUnit.value.weight.precision
    // )
    row.mete = toPrecision(row.singleMete * row.quantity, baseUnit.value.weight.precision)
    if (row.mete > row.maxMete && row.mete > row.singleMete) {
      row.mete = toPrecision(Math.min(row.maxMete, row.singleMete), baseUnit.value.weight.precision)
    }
  } else {
    row.mete = undefined
  }
}

// 使用草稿/修改时，为数据设置监听
function setFormCallback(form) {
  form.list = form.list.map((v) => reactive(v))
  const trigger = watch(
    [tableRef, headerRef, baseUnit],
    ([tRef, hRef, bu]) => {
      if (tRef && hRef && bu) {
        // 将相同的材料设置为同一个对象，便于计算
        extractSource(form.list)
        initCheckOverMaxWeight(form.list)
        // 初始化选中数据
        form.list.forEach((row) => {
          rowWatch(row)
          if (row.boolReturns && isNotBlank(row.list)) {
            row.list.forEach(k => {
              childrenWatch(k)
            })
          }
          checkOverSource(row)
        })
        nextTick(() => {
          trigger() // 执行一次后取消当前监听
          headerRef.value.calcAllQuantity()
          headerRef.value.calcAllWeight()
        })
      }
    },
    { immediate: true, deep: true }
  )
  fixMaxHeight()
}

function childrenValidate() {
  const key = []
  errorKey.value = []
  form.list.forEach(v => {
    if (v.boolReturns) {
      if (isNotBlank(v.list)) {
        v.list.forEach(k => {
          if (!k.length || !k.width || !k.quantity || !k.mete) {
            if (!key.includes(k.pid)) {
              key.push(k.pid)
            }
          }
        })
      } else {
        if (!key.includes(v.uid)) {
          key.push(v.uid)
        }
      }
    }
  })
  if (isNotBlank(key)) {
    errorKey.value = key
    expandRowKeys.value = key
    ElMessage.error('表格展开项红色部分余料退库明细填写不全，请修正')
    return false
  } else {
    return true
  }
}

function meteOver() {
  const key = []
  errorKey.value = []
  form.list.forEach(v => {
    if (v.boolReturns) {
      if (v.detailMete > v.mete) {
        if (!key.includes(v.uid)) {
          key.push(v.uid)
        }
      }
    }
  })
  if (isNotBlank(key)) {
    errorKey.value = key
    expandRowKeys.value = key
    ElMessage.error('表格展开项余料不能超过总量，请修正')
    return false
  } else {
    return true
  }
}

function formSubmit() {
  let next = childrenValidate()
  if (next) {
    next = meteOver()
  }
  if (next) {
    cu.submit()
  }
}
</script>

<style lang="scss" scoped>
.el-table {
  ::v-deep(.current-row > td.el-table__cell) {
    --el-table-current-row-background-color: #d7ffef;
  }
}
.child-table{
 ::v-deep(th.el-table__cell.is-leaf) {
    background: #e5f9f1 !important;
  }
  ::v-deep(.el-table__empty-block){
    background: #e5f9f1 !important;
  }
  ::v-deep(td.el-table__cell){
    background: #e5f9f1 !important;
  }
}
.error-table{
 ::v-deep(th.el-table__cell.is-leaf) {
    background: #ff000021 !important;
  }
  ::v-deep(.el-table__empty-block){
    background: #ff000021 !important;
  }
  ::v-deep(td.el-table__cell){
    background: #ff000021 !important;
  }
}
::v-deep(.table-expand-container){
  padding-right:0;
  padding-left:0;
}
::v-deep(.el-textarea__inner){
  background:none;
}
</style>
