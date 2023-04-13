<template>
  <common-drawer ref="drawerRef" title="QHSE事件曝光" v-model="drawerVisible" direction="rtl" :before-close="handleClose" size="55%">
    <template #titleRight>
      <export-button type="warning" size="mini" :params="{ id: detailData.project?.id }" :fn="downloadZipGet">下载</export-button>
    </template>
    <template #content>
      <el-descriptions v-loading="listLoading" :data="detailData" :column="2" size="large" border>
        <el-descriptions-item align="center" label="项目">
          <span>{{ detailData.project?.serialNumber }}-{{ detailData.project?.shortName }}</span>
        </el-descriptions-item>
        <el-descriptions-item align="center" label="单体">
          <span>{{ detailData.monomer?.name }}</span>
        </el-descriptions-item>
        <el-descriptions-item align="center" label="制造人">
          <span>{{ detailData.monomer?.name }}</span>
        </el-descriptions-item>
        <el-descriptions-item align="center" label="质检人">
          <span>{{ detailData.monomer?.name }}</span>
        </el-descriptions-item>
        <el-descriptions-item align="center" label="问题类型">
          <span>{{ problemTypeEnum.VL[detailData.type] }}</span>
        </el-descriptions-item>
        <el-descriptions-item align="center" label="工序">
          <span>{{ detailData.processName }}</span>
        </el-descriptions-item>
        <el-descriptions-item align="center" label="构件编号" :span="2">
          <span>{{ detailData.productSerialNumber }}</span>
        </el-descriptions-item>
        <el-descriptions-item align="center" label="问题类型">
          <span>{{ detailData.qualityTypeListStr }}</span>
        </el-descriptions-item>
        <el-descriptions-item align="center" label="日期">
          <span>{{ parseTime(detailData.createTime, '{y}-{m}-{d} {h}:{i}') }}</span>
        </el-descriptions-item>
        <el-descriptions-item align="center" label="问题图片" :span="2">
          <div class="imgs-box">
            <el-image
              v-for="url in detailData?.attachmentDTOS"
              :preview-src-list="detailData?.imgUrls"
              :initial-index="1"
              :key="url.id"
              :src="url.tinyImageUrl"
              lazy
            ></el-image>
          </div>
        </el-descriptions-item>
        <el-descriptions-item align="center" label="整改人">
          <span>{{ detailData.rectifyName }}</span>
        </el-descriptions-item>
        <el-descriptions-item align="center" label="日期">
          <span>{{ parseTime(detailData.rectifyTime, '{y}-{m}-{d} {h}:{i}') }}</span>
        </el-descriptions-item>
        <el-descriptions-item align="center" label="整改图片" :span="2">
          <div class="imgs-box">
            <el-image
              v-for="url in detailData?.rectifyAttachmentDTOS"
              :preview-src-list="detailData?.rectifyImgUrls"
              :initial-index="1"
              :key="url.id"
              :src="url.tinyImageUrl"
              lazy
            ></el-image>
          </div>
        </el-descriptions-item>
        <el-descriptions-item align="center" label="罚款" :span="2">
          <span>{{ detailData.forfeit }}</span>
        </el-descriptions-item>
      </el-descriptions>
    </template>
  </common-drawer>
</template>

<script setup>
import { defineProps, defineEmits, ref } from 'vue'
import { parseTime } from '@/utils/date'
import { problemTypeEnum } from '@enum-ms/production'
import useVisible from '@compos/use-visible'
import ExportButton from '@comp-common/export-button/index.vue'

const drawerRef = ref()
const emit = defineEmits(['update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  detailData: {
    type: Object,
    default: () => {}
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible' })

const listLoading = ref(false)
</script>
<style lang="scss" scoped>
.imgs-box {
  & > .el-image {
    width: 200px;
    height: 180px;
    border: 2px solid #dcdfe6;
    border-radius: 6px;
    background-color: white;
    cursor: pointer;
    + .el-image {
      margin-left: -40px;
    }
  }
}
</style>
