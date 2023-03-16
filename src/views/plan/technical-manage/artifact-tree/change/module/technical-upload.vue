<template>
  <div class="technical-upload-content" :style="heightStyle">
    <el-card>
      <div class="box-content" v-for="item in contentSetting" :key="item.title">
        <div class="upload-title">
          <span># {{ item.title }}</span>
        </div>
        <common-table :data="item.table" style="width: 100%">
          <el-table-column label="序号" type="index" align="center" width="60" />
          <el-table-column prop="type" show-overflow-tooltip label="类型" width="100"/>
          <el-table-column prop="uploadSpec" show-overflow-tooltip label="文件上传规范"/>
          <el-table-column prop="changeQuantity" show-overflow-tooltip label="变更总数" width="120"/>
          <el-table-column prop="matchQuantity" show-overflow-tooltip label="文件匹配总数" width="120"/>
        </common-table>
      </div>
    </el-card>
  </div>
</template>

<script setup>
import { defineProps } from 'vue'

defineProps({
  heightStyle: {
    type: String
  }
})

const contentSetting = [
  {
    title: '构件技术成果上传',
    table: [
      {
        type: '构件图纸',
        uploadSpec: '文件导入支持PDF格式、DWG格式、ZIP压缩包格式（内容为PDF格式、DWG格式）',
        changeQuantity: 0,
        matchQuantity: 0
      },
      { type: '智造构件XML', uploadSpec: '文件导入支持XML格式、ZIP压缩包格式（内容为XML格式）', changeQuantity: 0, matchQuantity: 0 }
    ]
  },
  {
    title: '部件技术成果上传',
    table: [
      {
        type: '部件图纸',
        uploadSpec: '文件导入支持PDF格式、DWG格式、ZIP压缩包格式（内容为PDF格式、DWG格式）',
        changeQuantity: 0,
        matchQuantity: 0
      },
      { type: '数控文件', uploadSpec: '文件导入仅支持NC1格式、ZIP压缩包格式（内容为NC1格式）', changeQuantity: 0, matchQuantity: 0 }
    ]
  },
  {
    title: '零件技术成果上传',
    table: [
      {
        type: '零件图纸',
        uploadSpec: '文件导入支持PDF格式、DWG格式、ZIP压缩包格式（内容为PDF格式、DWG格式）',
        changeQuantity: 0,
        matchQuantity: 0
      },
      { type: '数控文件', uploadSpec: '文件导入仅支持DXF格式、ZIP压缩包格式（内容为DXF格式）', changeQuantity: 0, matchQuantity: 0 }
    ]
  }
]
</script>

<style lang="scss" scoped>
::-webkit-scrollbar {
  /*滚动条整体样式*/
  width: 4px; /*高宽分别对应横竖滚动条的尺寸*/
  height: 4px;
}

.box-content {
  margin-bottom: 20px;
}
.upload-title {
  font-size: 18px;
  color: rgb(51, 51, 51);
  font-weight: bold;
  white-space: nowrap;
  letter-spacing: 0px;
  word-break: normal;
  margin-bottom: 10px;
}
</style>
