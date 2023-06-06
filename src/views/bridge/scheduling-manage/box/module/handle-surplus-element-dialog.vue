<template>
  <common-dialog title="多余单元件处理" v-model="dialogVisible" width="95%" fullscreen :before-close="handleClose">
    <template #titleRight>
      <slot name="saveBtn"></slot>
    </template>
    <div class="wrap">
      <div class="wrap-left">
        <common-table
          :data="surplusList"
          :max-height="maxHeight"
          :stripe="false"
          return-source-data
          style="width: 100%"
          :cell-class-name="wrongCellMask"
          highlight-current-row
          @current-change="handleElementRowClick"
        >
          <el-table-column label="序号" type="index" align="center" width="60" />
          <!-- <el-table-column prop="attributeType" :show-overflow-tooltip="true" label="属性" width="90" align="center">
            <template #default>
              <el-tag type="warning">单元件</el-tag>
            </template>
          </el-table-column> -->
          <!-- <el-table-column prop="elementConfigName" :show-overflow-tooltip="true" label="单元件类型" min-width="100" align="center" /> -->
          <el-table-column prop="serialNumber" :show-overflow-tooltip="true" label="编号" min-width="100" align="center" />
          <el-table-column prop="specification" :show-overflow-tooltip="true" label="规格" min-width="120" align="center" />
          <el-table-column prop="length" :show-overflow-tooltip="true" label="长度（mm）" min-width="90" align="center" />
          <el-table-column prop="netWeight" :show-overflow-tooltip="true" label="重量（kg）" min-width="90" align="center" />
          <el-table-column prop="quantity" :show-overflow-tooltip="true" label="数量" width="90" align="center" />
          <el-table-column prop="unHandleQuantity" :show-overflow-tooltip="true" label="未匹配数量" width="90" align="center">
            <template #default="{ row }">
              <span :class="row.unHandleQuantity ? 'tc-danger' : 'tc-success'">{{ row.unHandleQuantity }}</span>
            </template>
          </el-table-column>
        </common-table>
      </div>
      <div class="wrap-item">
        <common-table
          ref="boxTableRef"
          :data="boxList"
          :max-height="maxHeight"
          :stripe="false"
          style="width: 100%"
          return-source-data
          highlight-current-row
          @current-change="handleBoxClickRow"
          @selection-change="handleBoxSelectionChange"
        >
          <el-table-column type="selection" width="55" align="center" />
          <el-table-column prop="name" :show-overflow-tooltip="true" label="分段名称" min-width="100" align="center" />
          <el-table-column prop="serialNumber" :show-overflow-tooltip="true" label="分段编号" min-width="100" align="center" />
          <el-table-column prop="specification" :show-overflow-tooltip="true" label="规格" min-width="120" align="center" />
          <el-table-column prop="length" :show-overflow-tooltip="true" label="长度（mm）" min-width="90" align="center" />
          <el-table-column prop="netWeight" :show-overflow-tooltip="true" label="重量（kg）" min-width="90" align="center" />
          <el-table-column :show-overflow-tooltip="true" label="可匹配数量" width="90" align="center">
            <template #default="{ row }">
              <el-tooltip content="未排产数量" placement="top">
                <span>{{ row.boxCanHandleQ }}</span>
              </el-tooltip>
              <template v-if="row.schedulingCanHandleQ">
                <span> | </span>
                <el-tooltip content="已排产数量" placement="top">
                  <span>{{ row.schedulingCanHandleQ }}</span>
                </el-tooltip>
              </template>
            </template>
          </el-table-column>
          <el-table-column prop="quantity" :show-overflow-tooltip="true" label="匹配数量" width="100" align="center">
            <template #default="{ row }">
              <el-input-number
                v-if="row.boxCanHandleQ"
                v-model="row.editQuantity"
                :step="1"
                :min="0"
                :max="Math.min(curElement.unHandleQuantity + row.boxCanHandleQ, row.boxCanHandleQ)"
                :precision="0"
                size="mini"
                controls-position="right"
                style="width: 100%"
              />
              <span v-else>{{ row.quantity }}</span>
            </template>
          </el-table-column>
        </common-table>
      </div>
      <div class="wrap-right">
        <common-table
          ref="schedulingTableRef"
          :data="schedulingList"
          :max-height="maxHeight"
          :stripe="false"
          return-source-data
          style="width: 100%"
          @select="handleSchedulingSelectionChange"
          @select-all="handleSchedulingSelectionChange"
        >
          <el-table-column type="selection" width="55" align="center" />
          <el-table-column prop="groups.name" :show-overflow-tooltip="true" label="生产组" min-width="100" align="center" />
          <el-table-column prop="quantity" :show-overflow-tooltip="true" label="可匹配数量" width="90" align="center" />
          <el-table-column prop="editQuantity" :show-overflow-tooltip="true" label="匹配数量" width="100" align="center">
            <template #default="{ row }">
              <el-input-number
                v-model="row.editQuantity"
                :step="1"
                :min="0"
                :max="Math.min(curElement.unHandleQuantity + row.editQuantity, row.quantity)"
                :precision="0"
                size="mini"
                controls-position="right"
                style="width: 100%"
              />
            </template>
          </el-table-column>
        </common-table>
      </div>
    </div>
  </common-dialog>
</template>

<script setup>
import { defineEmits, defineProps, ref, watch, defineExpose } from 'vue'

import { isNotBlank, isBlank, deepClone } from '@data-type/index'
import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import useTableValidate from '@compos/form/use-table-validate'

const boxTableRef = ref()
const schedulingTableRef = ref()
const emit = defineEmits(['update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  surplusList: {
    type: Array,
    default: () => []
  },
  groupsId: {
    type: [String, Number]
  }
})

const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook })
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-dialog__header'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true,
    minHeight: 300,
    navbar: false
  },
  dialogVisible
)

const validateQuantity = (value, row) => {
  if (value) return false
  return true
}

const tableRules = {
  unHandleQuantity: [{ validator: validateQuantity, message: '请选择匹配数量', trigger: 'change' }]
}

const { tableValidate, wrongCellMask } = useTableValidate({ rules: tableRules }) // 表格校验

const curElement = ref({})
const elementList = ref([])
const curBox = ref({})
const boxList = ref([])
const schedulingList = ref([])

function showHook() {
  elementList.value = props.surplusList.map((v) => {
    v.unHandleQuantity = v.quantity
    v.selectBox = []
    rowElementWatch(v)
    return v
  })
}

function rowElementWatch(row) {
  watch(
    [() => row.selectBox],
    () => {
      calcElementHandleQuantity(row)
    },
    { deep: true }
  )
}

function calcElementHandleQuantity(row) {
  const _handleQuantity = row.selectBox.reduce((pre, cur) => {
    return pre + (cur?.allHandledQ || 0)
  }, 0)
  row.unHandleQuantity = row.quantity - _handleQuantity
}

function handleElementRowClick(val) {
  curElement.value = val
  boxTableRef.value?.setCurrentRow()
  schedulingList.value = []
  console.log(val, 'handleElementRowClick')
  const _selectBox = (val?.selectBox?.length && deepClone(val.selectBox)) || []
  boxList.value =
    val?.boxList.map((v) => {
      const findObj = _selectBox.find((o) => o.id === v.id)
      console.log(_selectBox, v, findObj)
      if (isNotBlank(findObj)) {
        v.editQuantity = findObj.editQuantity
        v.allHandledQ = findObj.allHandledQ
        v.boxCanHandleQ = findObj.boxCanHandleQ
        v.schedulingCanHandleQ = findObj.schedulingCanHandleQ
        v.selectScheduling = findObj.selectScheduling
        boxTableRef.value?.toggleRowSelection(v)
      } else {
        v.editQuantity = 0
        v.selectScheduling = []
        v.schedulingCanHandleQ = v.boxSchedulingList.reduce((pre, cur) => {
          return pre + cur.quantity
        }, 0)
        v.boxCanHandleQ = v.quantity - v.schedulingCanHandleQ
      }
      rowBoxWatch(v)
      return v
    }) || []
}

function rowBoxWatch(row) {
  watch(
    [() => row.editQuantity, () => row.selectScheduling],
    () => {
      calcBoxHandledQuantity(row)
    },
    { deep: true }
  )
}

function handleBoxClickRow(val) {
  if (isBlank(val)) return
  curBox.value = val
  console.log(val, 'handleBoxClickRow')
  const _selectScheduling = (val?.selectScheduling?.length && deepClone(val.selectScheduling)) || []
  schedulingList.value = val.boxSchedulingList.map((v) => {
    const findObj = _selectScheduling.find((o) => o.id === v.id)
    if (isNotBlank(findObj)) {
      v.editQuantity = findObj.editQuantity
      schedulingTableRef.value?.toggleRowSelection(v, true)
    } else {
      v.editQuantity = 0
    }
    rowSchedulingWatch(v)
    return v
  })
}

function rowSchedulingWatch(row) {
  watch(
    () => row.editQuantity,
    () => {
      calcBoxHandledQuantity(curBox.value)
    }
  )
}

function calcBoxHandledQuantity(row) {
  const _schedulingHandledQ = row.selectScheduling.reduce((pre, cur) => {
    return pre + cur.editQuantity
  }, 0)
  row.allHandledQ = _schedulingHandledQ + row.editQuantity
}

function handleBoxSelectionChange(selections) {
  console.log(selections, 'handleBoxSelectionChange')
  curElement.value.selectBox = selections
}

function handleSchedulingSelectionChange(selections) {
  curBox.value.selectScheduling = selections
  if (selections.length && curBox.value?.boxSchedulingList?.length) {
    boxTableRef.value?.toggleRowSelection(curBox.value, true)
  } else {
    boxTableRef.value?.toggleRowSelection(curBox.value, false)
  }
}

// 校验
function handleValidate() {
  const { validResult, dealList } = tableValidate(elementList.value)
  const _list = []
  if (!validResult) return
  for (let i = 0; i < dealList.length; i++) {
    const v = dealList[i]
    for (let x = 0; x < v.selectBox.length; x++) {
      const a = v.selectBox[x]
      if (a.editQuantity) {
        _list.push({
          groupsId: props.groupsId,
          productId: a.id,
          quantity: a.editQuantity
        })
      }
      if (a?.selectScheduling) {
        for (let y = 0; y < a.selectScheduling.length; y++) {
          const s = a.selectScheduling[y]
          if (s.editQuantity) {
            _list.push({
              groupsId: props.groupsId,
              productId: a.id,
              schedulingId: s.id,
              quantity: s.editQuantity
            })
          }
        }
      }
    }
  }
  return _list
}

defineExpose({
  handleValidate
})
</script>

<style lang="scss" scoped>
.wrap {
  display: flex;

  .wrap-left {
    width: 670px;
    margin-right: 10px;
  }

  .wrap-item {
    flex: 1;
    margin-right: 10px;
  }
  .wrap-right {
    width: 400px;
  }
}
::-webkit-scrollbar {
  width: 6px;
  height: 6px;
}
::-webkit-scrollbar-thumb {
  border-radius: 6px;
}
</style>
