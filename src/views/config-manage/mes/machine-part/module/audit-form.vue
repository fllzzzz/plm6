<template>
  <common-drawer
    ref="drawerRef"
    append-to-body
    v-model="visible"
    :before-close="handleClose"
    :title="'部件特征定义审核'"
    :center="false"
    :close-on-click-modal="false"
    size="800px"
  >
    <template #titleRight>
      <common-button :loading="loading" type="primary" size="mini" @click="onSubmit(auditTypeEnum.PASS.V)">通过</common-button>
      <common-button :loading="loading" type="warning" size="mini" @click="onSubmit(auditTypeEnum.REJECT.V)">驳回</common-button>
    </template>
    <template #content>
      <el-form ref="formRef" size="small" label-width="140px">
        <div class="detail-header">
          <el-form-item label="是否型材" prop="classifyIds">
            <cell-change-preview v-if="detailInfo?.boolSectionSteel!==detailInfo?.newBoolSectionSteel" :old="whetherEnum.VL[detailInfo.boolSectionSteel]" :new="whetherEnum.VL[detailInfo.newBoolSectionSteel]" />
            <span v-else>{{whetherEnum.VL[detailInfo.boolSectionSteel]}}</span>
            <br />
            <template v-if="detailInfo?.boolSectionSteel">
              <cell-change-preview v-if="!compareArray(detailInfo?.classifyIds,detailInfo?.newClassifyIds)" :old="detailInfo.classifyNames" :new="detailInfo.newClassifyNames" />
              <span v-else>{{detailInfo.classifyNames}}</span>
            </template>
          </el-form-item>
          <el-form-item label="代表部件类型" prop="name">
            <cell-change-preview v-if="detailInfo?.name!==detailInfo?.name" :old="detailInfo.name" :new="detailInfo.newName" />
            <span v-else>{{detailInfo.name}}</span>
          </el-form-item>
          <el-form-item label="排序" prop="sort">
            <cell-change-preview v-if="detailInfo?.sort!==detailInfo?.sort" :old="detailInfo.sort" :new="detailInfo.newSort" />
            <span v-else>{{detailInfo.sort}}</span>
          </el-form-item>
        </div>
        <common-table
          ref="detailRef"
          border
          :data="detailInfo.newAssembleSpecList"
          :max-height="maxHeight"
          style="width: 100%; margin-top: 10px"
          class="table-form"
          return-source-data
          :showEmptySymbol="false"
        >
          <el-table-column label="序号" type="index" align="center" width="50" />
          <el-table-column key="specPrefix" prop="specPrefix" label="部件规格前缀(大写字母或中文)" align="center">
            <template v-slot="scope">
              <table-cell-tag :name="changeTypeEnum.VL[scope.row.changeType]" :color="changeTypeEnum.V[scope.row.changeType]?.COLOR" :offset="15" />
              <span v-if="scope.row.specPrefix!==scope.row.newSpecPrefix">
                <cell-change-preview :old="scope.row.specPrefix" :new="scope.row.newSpecPrefix" />
              </span>
              <span v-else>{{scope.row.specPrefix}}</span>
            </template>
          </el-table-column>
          <el-table-column key="specIndex" prop="specIndex" label="索引" align="center">
            <template v-slot="scope">
               <span v-if="scope.row.specIndex!==scope.row.newSpecIndex">
                <cell-change-preview :old="scope.row.specIndex || '全部'" :new="scope.row.newSpecIndex || '全部'" />
              </span>
              <span v-else>{{scope.row.specIndex || '全部'}}</span>
            </template>
          </el-table-column>
        </common-table>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { audit } from '@/api/config/system-config/machine-part-config'
import { defineProps, defineEmits, ref } from 'vue'

import { changeTypeEnum } from '@enum-ms/mes'
import { whetherEnum } from '@enum-ms/common'
import { auditTypeEnum } from '@enum-ms/contract'
import { compareArray } from '@/utils/data-type/array'
import { ElNotification } from 'element-plus'
import cellChangePreview from '@comp-common/cell-change-preview'
import useVisible from '@compos/use-visible'
import useMaxHeight from '@compos/use-max-height'

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  showType: {
    type: String,
    default: 'detail'
  },
  detailInfo: {
    type: Object,
    default: () => {}
  }
})

const formRef = ref()
const drawerRef = ref()
const loading = ref(false)

const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header', '.detail-header'],
    wrapperBox: ['.el-drawer__body', '.table-form'],
    navbar: false,
    extraHeight: 120
  },
  () => drawerRef.value.loaded
)

async function onSubmit(type) {
  loading.value = true
  try {
    await audit({
      id: props.detailInfo.lastChangeId,
      auditType: type
    })
    ElNotification({ title: '审核成功', type: 'success' })
    emit('success')
    handleClose()
  } catch (error) {
    console.log('提交失败', error)
  } finally {
    loading.value = false
  }
}
</script>

<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
.process-container {
  display: flex;
  flex-direction: row;
  justify-content: flex-start;
  align-items: flex-end;
  .process-box {
    display: flex;
    // flex-direction: column;
    // justify-content: flex-start;
    // align-items: flex-start;
    .process-drawer {
      display: flex;
      // flex-direction: row;
      // justify-content: flex-start;
      // align-items: center;
      // margin-bottom: 10px;
    }
  }
}
</style>
