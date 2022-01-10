<template>
  <common-dialog
    custom-class="reject-application-preview"
    title="退货预览"
    append-to-body
    v-model="dialogVisible"
    width="1600px"
    :before-close="handleClose"
    :top="'5vh'"
  >
    <template #titleRight>
      <common-button :loading="loading" :disabled="isBlank(rejectList)" type="primary" size="mini" @click="submit">提 交</common-button>
    </template>
    <el-form :model="form" :disabled="loading">
      <common-table :data="rejectList" :max-height="maxHeight" empty-text="未做改动" row-key="id">
        <!-- 基础信息 -->
        <material-base-info-columns :basic-class="basicClass" />
        <!-- 次要信息 -->
        <material-secondary-info-columns :basic-class="basicClass" />
        <!-- 单位及其数量 -->
        <material-unit-quantity-columns :basic-class="basicClass" outbound-type-mode />
        <!-- 仓库位置信息 -->
        <warehouse-info-columns show-project />
      </common-table>
      <el-input
        v-model="form.remark"
        :rows="2"
        :autosize="{ minRows: 2, maxRows: 2 }"
        type="textarea"
        placeholder="备注"
        maxlength="200"
        show-word-limit
        style="margin-top: 10px"
      />
    </el-form>
  </common-dialog>
</template>

<script setup>
import { rejectApplication } from '@/api/wms/material-reject/raw-material/application'
import { defineEmits, defineProps, ref, watch } from 'vue'
import { isBlank } from '@data-type'
import { obj2arr } from '@/utils/convert/type'
import { deepClone, toFixed } from '@/utils/data-type'
import { measureTypeEnum } from '@/utils/enum/modules/wms'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

import materialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import materialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import materialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import warehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'
import { ElNotification } from 'element-plus'

const emit = defineEmits(['success', 'update:visible'])

const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  basicClass: {
    type: Number
  },
  inboundId: {
    type: Number,
    required: false
  },
  // 退货信息
  rejectInfo: {
    type: Object,
    default: () => {
      return {}
    }
  }
})

const loading = ref(false)
const rejectList = ref([])
const form = ref({
  remark: undefined
})
const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible' })
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.reject-application-preview',
    extraBox: ['.el-dialog__header'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true
  },
  dialogVisible
)

watch(
  [() => props.visible, () => props.rejectInfo],
  ([visible, info]) => {
    if (visible) dataFormat(info)
  },
  { immediate: true, deep: true }
)

// 格式装换
function dataFormat(info) {
  const list = []
  for (const inboundMaterialId in info) {
    // 具体物料退货信息
    const item = info[inboundMaterialId]
    if (item && item.KV && item.material) {
      const sourceMaterial = item.material
      const rjArr = obj2arr(item.KV)
      const formatArr = rjArr.map((rj) => {
        const _material = deepClone(rj)
        _material.inboundMaterialId = inboundMaterialId
        // 存在计量单位，设置计量量
        if (sourceMaterial.measureUnit) {
          _material.quantity =
            sourceMaterial.outboundUnitType === measureTypeEnum.MEASURE.V
              ? rj.rejectNumber
              : toFixed(rj.rejectNumber * sourceMaterial.accountingUnitNet, sourceMaterial.measurePrecision)
        }
        // 设置核算量
        _material.mete =
          sourceMaterial.outboundUnitType === measureTypeEnum.ACCOUNTING.V
            ? rj.rejectNumber
            : toFixed(rj.rejectNumber * sourceMaterial.unitNet, sourceMaterial.accountingPrecision)

        return _material
      })
      list.push.apply(list, formatArr)
    }
  }
  rejectList.value = list
}

async function submit() {
  try {
    loading.value = true
    const data = {
      remark: form.value.remark,
      inboundId: props.inboundId,
      list: deepClone(rejectList.value)
    }
    data.list = await numFmtByBasicClass(data.list, { toSmallest: true, toNum: true })
    await rejectApplication(data)
    handleClose() // 关闭窗口
    emit('success')
    ElNotification({ title: '提交成功', type: 'success' })
  } catch (error) {
    console.log('退货', error)
  } finally {
    loading.value = false
  }
}
</script>
