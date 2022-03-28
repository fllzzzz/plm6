<template>
  <popover-confirm v-if="!props.number" confirm-button-text="打印" @confirm="toPrintLabel" @cancel="cancelPrintLabel">
    <template #default>
      <div class="flex-rsc">
        <span style="flex: none; font-weight: bold">打印份数：</span>
        <common-input-number v-model.number="currentNumber" :min="1" :max="999" size="mini" style="width: 120px" />
      </div>
    </template>
    <template #reference>
      <common-button v-bind="$attrs" icon="el-icon-printer" type="success" size="mini" />
    </template>
  </popover-confirm>
  <common-button v-bind="$attrs" v-else icon="el-icon-printer" type="success" size="mini" @click="toPrintLabel" />
</template>

<script setup>
import { addPrintRecord } from '@/api/wms/material-label-print/index'

import { ref, defineEmits, defineProps } from 'vue'
import { printMaterialLabel } from '@/utils/print/wms-material-label'
import { isBlank } from '@data-type/index'

import PopoverConfirm from '@/components-system/common/popover-confirm.vue'

const emit = defineEmits(['printed-success'])

const props = defineProps({
  material: {
    type: Object
  },
  number: {
    type: Number
  },
  copies: {
    type: Number,
    default: 1
  },
  submitPrintRecord: {
    type: Boolean,
    default: false
  }
})

// 打印份数
const currentNumber = ref(1)

// 打印标签
async function toPrintLabel() {
  if (isBlank(props.material)) {
    throw Error('物料信息不存在')
  }
  const startTime = new Date().getTime()
  if (!props.number) {
    await printMaterialLabel({ material: props.material, copies: currentNumber.value * props.copies })
    currentNumber.value = 1
  } else {
    await printMaterialLabel({ material: props.material, copies: props.number * props.copies })
  }
  if (props.submitPrintRecord) {
    const endTime = new Date().getTime()
    await addRecord({
      material: props.material,
      number: props.number || currentNumber.value,
      copies: props.copies,
      startTime,
      endTime
    })
  } else {
    emit('printed-success')
  }
}

// 添加打印记录
async function addRecord({ material, number, copies, startTime, endTime }) {
  const receiptMaterialId = material.id
  if (!receiptMaterialId || !number) return
  try {
    await addPrintRecord({ receiptMaterialId, number, copies, startTime, endTime })
    emit('printed-success')
    setTimeout(() => {
      if (material.printedNumber) material.printedNumber += number
    }, 0)
  } catch (error) {
    console.log('添加打印记录失败', error)
  }
}

// 取消打印
function cancelPrintLabel() {
  currentNumber.value = 1
}
</script>
