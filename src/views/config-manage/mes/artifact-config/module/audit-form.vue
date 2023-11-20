<template>
  <common-drawer
    ref="drawerRef"
    append-to-body
    v-model="visible"
    :before-close="handleClose"
    :title="'构件特征定义审核'"
    :center="false"
    :close-on-click-modal="false"
    size="800px"
  >
    <template #titleRight>
      <common-button :loading="loading" type="primary" size="mini" @click="onSubmit(auditTypeEnum.PASS.V)">通过</common-button>
      <common-button :loading="loading" type="warning" size="mini" @click="onSubmit(auditTypeEnum.REJECT.V)">驳回</common-button>
    </template>
    <template #content>
      <el-form ref="formRef" size="small" label-width="140px" :max-height="maxHeight">
        <el-form-item label="生产线" prop="productionLineTypeEnum">
          <div>
            {{artifactProductLineEnum.VL[detailInfo?.productionLineTypeEnum]}}
          </div>
        </el-form-item>
        <el-form-item label="构件类型" prop="artifactType" v-if="detailInfo?.productionLineTypeEnum === artifactProductLineEnum.TRADITION.V">
          <span>{{artifactTypeEnum.VL[detailInfo?.artifactType]}}</span>
        </el-form-item>
        <el-form-item label="次构件类型" prop="newSmallArtifactClassEnum" v-if="detailInfo.artifactType === artifactTypeEnum.SMALL.V">
          <cell-change-preview v-if="detailInfo?.smallArtifactClassEnum!==detailInfo?.newSmallArtifactClassEnum" :old="detailInfo?.smallArtifactClassEnum" :new="detailInfo?.newSmallArtifactClassEnum" />
          <span v-else>{{smallArtifactClassEnum.VL[detailInfo?.smallArtifactClassEnum]}}</span>
        </el-form-item>
        <el-form-item label="类型" prop="newParentType" v-if="detailInfo?.productionLineTypeEnum === artifactProductLineEnum.INTELLECT.V">
          <cell-change-preview v-if="detailInfo?.parentType!==detailInfo?.newParentType" :old="detailInfo?.parentType" :new="detailInfo?.newParentType" />
          <span v-else>{{intellectParentType.VL[detailInfo?.parentType]}}</span>
        </el-form-item>
        <el-form-item label="类型命名" prop="newClassificationName" v-if="detailInfo?.productionLineTypeEnum">
          <cell-change-preview v-if="detailInfo?.classificationName!==detailInfo?.newClassificationName" :old="detailInfo?.classificationName" :new="detailInfo?.newClassificationName" />
          <span v-else>{{detailInfo?.classificationName}}</span>
        </el-form-item>
        <el-form-item label="定义代码" prop="newDefinitionWord" v-if="detailInfo?.productionLineTypeEnum === artifactProductLineEnum.INTELLECT.V">
          <cell-change-preview v-if="detailInfo?.definitionWord!==detailInfo?.newDefinitionWord" :old="detailInfo?.definitionWord" :new="detailInfo?.newDefinitionWord" />
          <span v-else>{{detailInfo?.definitionWord}}</span>
        </el-form-item>
        <el-form-item label="排序" prop="sort">
          <cell-change-preview v-if="detailInfo?.sort!==detailInfo?.newSort" :old="detailInfo?.sort" :new="detailInfo?.newSort" />
          <span v-else>{{detailInfo?.sort}}</span>
        </el-form-item>
        <el-form-item label="构件规格前缀" prop="specPrefixList">
          <el-row>
            <el-col :span="6">更改类型</el-col>
            <el-col :span="18">内容</el-col>
          </el-row>
          <el-row v-for="(item, index) in detailInfo?.newSpecPrefixList" :key="index">
            <el-col :span="6"><el-tag :type="changeTypeEnum.V[item.changeType]?.TAG">{{changeTypeEnum.VL[item.changeType]}}</el-tag></el-col>
            <span v-if="item.changeType===changeTypeEnum.MODIFY.V">
              <cell-change-preview :old="item.specPrefix" :new="item.newSpecPrefix" />
            </span>
            <span v-else :style="`color:${changeTypeEnum.V[item.changeType]?.COLOR}`">{{item.newSpecPrefix}}</span>
          </el-row>
        </el-form-item>
        <el-form-item label="编号类型索引" prop="serialNumberPrefixList" v-if="detailInfo.artifactType === artifactTypeEnum.SMALL.V">
          <el-row>
            <el-col :span="6">更改类型</el-col>
            <el-col :span="18">内容</el-col>
          </el-row>
          <el-row v-for="(item, index) in detailInfo?.newSerialNumberPrefixList" :key="index">
            <el-col :span="6"><el-tag :type="changeTypeEnum.V[item.changeType]?.TAG">{{changeTypeEnum.VL[item.changeType]}}</el-tag></el-col>
            <span v-if="item.changeType===changeTypeEnum.MODIFY.V">
              <cell-change-preview :old="item.serialNumberPrefix" :new="item.newSerialNumberPrefix" />
            </span>
            <span v-else :style="`color:${changeTypeEnum.V[item.changeType]?.COLOR}`">{{item.newSerialNumberPrefix}}</span>
          </el-row>
        </el-form-item>
        <el-form-item label="打码方式" prop="codingType" v-if="detailInfo?.productionLineTypeEnum === artifactProductLineEnum.TRADITION.V">
           <cell-change-preview v-if="detailInfo?.codingType!==detailInfo?.newCodingType" :old="codingTypeEnum.VL[detailInfo.codingType]" :new="codingTypeEnum.VL[detailInfo.newCodingType]" />
          <span>{{codingTypeEnum.VL[detailInfo.newCodingType]}}</span>
        </el-form-item>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { audit } from '@/api/config/system-config/artifact-config'
import { defineProps, defineEmits, ref } from 'vue'
import { auditTypeEnum } from '@enum-ms/contract'
import { ElNotification } from 'element-plus'
import cellChangePreview from '@comp-common/cell-change-preview'
import {
  artifactProductLineEnum,
  intellectParentType,
  artifactTypeEnum,
  smallArtifactClassEnum,
  changeTypeEnum,
  codingTypeEnum
} from '@enum-ms/mes'

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
    wrapperBox: ['.el-drawer__body'],
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
