<template>
  <common-drawer
    ref="drawerRef"
    :title="`${productTypeText}：${detailInfo?.name ? detailInfo?.name + '-' : ''}${detailInfo?.serialNumber}`"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    size="60%"
  >
    <template #titleRight>
      <common-button
        v-permission="permission.edit"
        type="primary"
        size="mini"
        :disabled="!modifiedData.length"
        @click="previewVisible = true"
        >预览并保存</common-button
      >
    </template>
    <template #content>
      <div class="tag-content">
        <template v-for="item in processList" :key="item.id">
          <el-tag
            hit
            :effect="true ? (selectProcessId === item.id ? 'light' : 'plain') : item.selected ? 'light' : 'plain'"
            :type="true ? (selectProcessId === item.id ? 'success' : 'info') : item.selected ? 'success' : 'info'"
            @click="handleProcessSelect(item)"
          >
            {{ item.name }}
          </el-tag>
        </template>
        <span v-if="!processList || !processList.length" style="font-size: 12px; color: red; margin-left: 10px">*暂未获取到工序</span>
      </div>
      <common-table
        ref="tableRef"
        v-loading="loading"
        return-source-data
        :show-empty-symbol="false"
        :data-format="dataFormat"
        :data="list"
        :max-height="maxHeight"
        row-key="rowId"
        style="width: 100%"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <belonging-info-columns showFactory showWorkshop showProductionLine showTeam />
        <el-table-column align="center" prop="prop" label="结算方式">
          <template #default="{ row }">
            <span>{{ row.wageQuotaTypeStr }}</span>
          </template>
        </el-table-column>
        <el-table-column align="center" prop="wages" label="定额单价"> </el-table-column>
        <el-table-column align="center" prop="price" label="修改后单价" width="200">
          <template #default="{ row }">
            <common-input-number v-model="row.price" placeholder="请输入单价" :precision="decimalPrecision.mes" :controls="false" style="width: 100%">
            </common-input-number>
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
  <edit-preview
    v-model:visible="previewVisible"
    :modified-data="modifiedData"
    :data="{
      processId: selectProcessId,
      productId: detailInfo?.id,
      productType: productType,
      organizationType: organizationType,
    }"
    @success="handleEditSuccess"
  />
</template>

<script setup>
import { process, teamPrice } from '@/api/mes/team-report/wages-adjust/index'
import { defineProps, defineEmits, ref, watch, computed, inject } from 'vue'

import { isNotBlank } from '@data-type/index'
import { wageQuotaTypeEnum } from '@enum-ms/mes'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import belongingInfoColumns from '@comp-mes/table-columns/belonging-info-columns'
import editPreview from './edit-preview.vue'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

const { decimalPrecision } = useDecimalPrecision()

const drawerRef = ref()
const emit = defineEmits(['update:visible', 'refresh'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  productType: {
    type: Number
  },
  productTypeText: {
    type: String
  },
  detailInfo: {
    type: Object
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', closeHook: beforeClose })

const permission = inject('permission')

const dataFormat = [['wageQuotaTypeStr', ['parse-enum', wageQuotaTypeEnum], { source: 'wageQuotaType' }]]

// 高度
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)

watch(
  () => props.visible,
  (visible) => {
    if (visible) {
      init()
    }
  },
  { immediate: true }
)

function init() {
  fetchProcess()
  hasEdit.value = false
}

const organizationType = inject('organizationType')
const loading = ref(false)
const previewVisible = ref(false)
const list = ref([])
const processList = ref([])
const selectProcessId = ref()
const hasEdit = ref(false)

const modifiedData = computed(() => {
  return list.value.filter((v) => isNotBlank(v.price))
})

async function fetchProcess() {
  try {
    const { content } = await process({
      id: props.detailInfo?.id,
      productType: props.productType
    })
    processList.value = content
    if (content && content.length) {
      selectProcessId.value = content[0]?.id
      fetchTeam()
    }
  } catch (error) {
    console.log('获取工序', error)
  }
}

function handleProcessSelect(item) {
  selectProcessId.value = item.id
  fetchTeam()
}

async function fetchTeam() {
  try {
    const { content } = await teamPrice({
      processId: selectProcessId.value,
      productId: props.detailInfo?.id,
      productType: props.productType,
      organizationType: organizationType
    })
    list.value = content
  } catch (error) {
    console.log('获取工序', error)
  }
}

function handleEditSuccess() {
  hasEdit.value = true
  handleClose()
}

function beforeClose() {
  selectProcessId.value = undefined
  list.value = []
  processList.value = []
  if (hasEdit.value) {
    emit('refresh')
  }
}
</script>

<style lang="scss" scoped>
.tag-content {
  display: flex;
  flex-direction: row;
  justify-content: flex-start;
  align-items: center;
  flex-wrap: wrap;
  box-sizing: border-box;
  .el-tag {
    width: 100px;
    text-align: center;
    margin: 0 10px 15px 0;
    cursor: pointer;
    text-overflow: ellipsis;
    white-space: nowrap;
    overflow: hidden;
  }
  // .el-tag--info {
  //   // border-color: #303133;
  //   // color: #303133;
  // }
}
</style>
