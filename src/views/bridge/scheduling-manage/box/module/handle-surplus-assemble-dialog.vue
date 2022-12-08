<template>
  <common-dialog title="多余部件处理" v-model="dialogVisible" width="95%" fullscreen :before-close="handleClose">
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
          @current-change="handleAssembleRowClick"
        >
          <el-table-column label="序号" type="index" align="center" width="60" />
          <!-- <el-table-column prop="attributeType" :show-overflow-tooltip="true" label="属性" width="90" align="center">
            <template #default>
              <el-tag type="warning">部件</el-tag>
            </template>
          </el-table-column> -->
          <!-- <el-table-column prop="assembleConfigName" :show-overflow-tooltip="true" label="部件类型" min-width="100" align="center" /> -->
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
          ref="artifactTableRef"
          :data="artifactList"
          :max-height="maxHeight"
          :stripe="false"
          style="width: 100%"
          return-source-data
          highlight-current-row
          @current-change="handleArtifactClickRow"
          @selection-change="handleArtifactSelectionChange"
        >
          <el-table-column type="selection" width="55" align="center" />
          <el-table-column prop="name" :show-overflow-tooltip="true" label="构件名称" min-width="100" align="center" />
          <el-table-column prop="serialNumber" :show-overflow-tooltip="true" label="构件编号" min-width="100" align="center" />
          <el-table-column prop="specification" :show-overflow-tooltip="true" label="规格" min-width="120" align="center" />
          <el-table-column prop="length" :show-overflow-tooltip="true" label="长度（mm）" min-width="90" align="center" />
          <el-table-column prop="netWeight" :show-overflow-tooltip="true" label="重量（kg）" min-width="90" align="center" />
          <el-table-column :show-overflow-tooltip="true" label="可匹配数量" width="90" align="center">
            <template #default="{ row }">
              <el-tooltip content="未排产数量" placement="top">
                <span>{{ row.artifactCanHandleQ }}</span>
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
                v-if="row.artifactCanHandleQ"
                v-model="row.editQuantity"
                :step="1"
                :min="0"
                :max="Math.min(curAssemble.unHandleQuantity + row.artifactCanHandleQ, row.artifactCanHandleQ)"
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
                :max="Math.min(curAssemble.unHandleQuantity + row.editQuantity, row.quantity)"
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

const artifactTableRef = ref()
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

const curAssemble = ref({})
const assembleList = ref([])
const curArtifact = ref({})
const artifactList = ref([])
const schedulingList = ref([])

function showHook() {
  assembleList.value = props.surplusList.map((v) => {
    v.unHandleQuantity = v.quantity
    v.selectArtifact = []
    rowAssembleWatch(v)
    return v
  })
}

function rowAssembleWatch(row) {
  watch(
    [() => row.selectArtifact],
    () => {
      calcAssembleHandleQuantity(row)
    },
    { deep: true }
  )
}

function calcAssembleHandleQuantity(row) {
  const _handleQuantity = row.selectArtifact.reduce((pre, cur) => {
    return pre + (cur?.allHandledQ || 0)
  }, 0)
  row.unHandleQuantity = row.quantity - _handleQuantity
}

function handleAssembleRowClick(val) {
  curAssemble.value = val
  artifactTableRef.value?.setCurrentRow()
  schedulingList.value = []
  console.log(val, 'handleAssembleRowClick')
  const _selectArtifact = (val?.selectArtifact?.length && deepClone(val.selectArtifact)) || []
  artifactList.value =
    val?.artifactList.map((v) => {
      const findObj = _selectArtifact.find((o) => o.id === v.id)
      console.log(_selectArtifact, v, findObj)
      if (isNotBlank(findObj)) {
        v.editQuantity = findObj.editQuantity
        v.allHandledQ = findObj.allHandledQ
        v.artifactCanHandleQ = findObj.artifactCanHandleQ
        v.schedulingCanHandleQ = findObj.schedulingCanHandleQ
        v.selectScheduling = findObj.selectScheduling
        artifactTableRef.value?.toggleRowSelection(v)
      } else {
        v.editQuantity = 0
        v.selectScheduling = []
        v.schedulingCanHandleQ = v.artifactSchedulingList.reduce((pre, cur) => {
          return pre + cur.quantity
        }, 0)
        v.artifactCanHandleQ = v.quantity - v.schedulingCanHandleQ
      }
      rowArtifactWatch(v)
      return v
    }) || []
}

function rowArtifactWatch(row) {
  watch(
    [() => row.editQuantity, () => row.selectScheduling],
    () => {
      calcArtifactHandledQuantity(row)
    },
    { deep: true }
  )
}

function handleArtifactClickRow(val) {
  if (isBlank(val)) return
  curArtifact.value = val
  console.log(val, 'handleArtifactClickRow')
  const _selectScheduling = (val?.selectScheduling?.length && deepClone(val.selectScheduling)) || []
  schedulingList.value = val.artifactSchedulingList.map((v) => {
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
      calcArtifactHandledQuantity(curArtifact.value)
    }
  )
}

function calcArtifactHandledQuantity(row) {
  const _schedulingHandledQ = row.selectScheduling.reduce((pre, cur) => {
    return pre + cur.editQuantity
  }, 0)
  row.allHandledQ = _schedulingHandledQ + row.editQuantity
}

function handleArtifactSelectionChange(selections) {
  console.log(selections, 'handleArtifactSelectionChange')
  curAssemble.value.selectArtifact = selections
}

function handleSchedulingSelectionChange(selections) {
  curArtifact.value.selectScheduling = selections
  if (selections.length && curArtifact.value?.artifactSchedulingList?.length) {
    artifactTableRef.value?.toggleRowSelection(curArtifact.value, true)
  } else {
    artifactTableRef.value?.toggleRowSelection(curArtifact.value, false)
  }
}

// 校验
function handleValidate() {
  const { validResult, dealList } = tableValidate(assembleList.value)
  const _list = []
  if (!validResult) return
  for (let i = 0; i < dealList.length; i++) {
    const v = dealList[i]
    for (let x = 0; x < v.selectArtifact.length; x++) {
      const a = v.selectArtifact[x]
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
