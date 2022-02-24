<template>
  <common-dialog
    custom-class="wage-quota"
    title="变更提交预览"
    append-to-body
    v-model="dialogVisible"
    width="1200px"
    :before-close="handleClose"
    :top="'5vh'"
  >
    <template #titleRight>
      <common-button :loading="loading" :disabled="isBlank(modifiedList)" type="primary" size="mini" @click="submit">保 存</common-button>
    </template>
    <common-table :data="modifiedList" :max-height="maxHeight" empty-text="未做改动" row-key="id">
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column key="name" prop="name" :show-overflow-tooltip="true" label="工序名称" width="140px" align="center" />
      <template v-for="item in wageQuotaTypeEnum.ENUM" :key="item.V">
        <el-table-column :label="`${item.L} (${item.unit})`" min-width="170px" align="center">
          <template #default="{ row }">
            <cell-change-preview :old="row.originPriceMap[item.V]" :new="row.priceMap[item.V]" />
          </template>
        </el-table-column>
      </template>
    </common-table>
  </common-dialog>
</template>

<script setup>
import { processWageQuotaSave as save } from '@/api/mes/production-config/wage-quota'
import { computed, defineEmits, defineProps, inject, ref } from 'vue'
import { wageQuotaTypeEnum } from '@enum-ms/mes'
import { isBlank, judgeSameValue } from '@data-type/index'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import { ElNotification } from 'element-plus'
import cellChangePreview from '@comp-common/cell-change-preview'

const emit = defineEmits(['saveSuccess', 'update:visible'])

const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  data: {
    type: Array,
    default: () => []
  }
})

const crud = inject('crud')
const modifiedList = computed(() => props.data.filter((v) => !judgeSameValue(v.originPriceMap, v.priceMap)))
const loading = ref(false)
const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible' })
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.wage-quota',
    extraBox: ['.el-dialog__header'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true
  },
  dialogVisible
)

async function submit() {
  try {
    loading.value = true
    const changeList = []
    modifiedList.value.forEach((v) => {
      for (const item in v.priceMap) {
        if (v.priceMap[item] !== v.originPriceMap[item]) {
          changeList.push({
            processId: v.id,
            wageQuotaType: item,
            price: v.priceMap[item],
            organizationType: crud?.query?.organizationType
          })
        }
      }
    })
    await save(changeList)
    handleClose() // 关闭窗口
    crud.refresh() // 刷新页面
    emit('saveSuccess')
    ElNotification({ title: '修改成功', type: 'success' })
  } catch (error) {
    console.log('工价定额批量修改', error)
  } finally {
    loading.value = false
  }
}
</script>
