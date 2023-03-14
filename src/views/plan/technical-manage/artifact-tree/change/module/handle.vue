<!-- 变更处理 -->
<template>
  <div class="handle-content" :style="heightStyle">
    <div class="left-con">
      <el-card v-for="(item, index) in changeInfo" :key="index" class="artifact-content">
        <template #header>
          <div class="index">{{ index + 1 }}</div>
          <span class="new-sn">{{ item.newArtifact?.serialNumber }}</span>
          <span class="old-sn">原构件编号：{{ item.oldArtifact?.serialNumber }}</span>
          <span class="right-btn">
            <div class="circle-button" style="border-color: #ffac00"></div>
            <div class="circle-button" style="border-color: #da0000; color: #da0000"><ElClose style="width: 85%" /></div>
          </span>
        </template>
        <div class="box-content">
          <div class="handle-title">
            <span># 基本信息</span>
          </div>
          <div style="display: flex">
            <artifact-change-table :newArtifact="item.newArtifact" :oldArtifact="item.oldArtifact" style="width: 750px" />
            <artifact-area-change-table :areaList="item.areaList" style="flex: 1; margin-left: 15px" />
          </div>
        </div>
        <div class="box-content">
          <div class="handle-title">
            <span># 部件变更设置（单构件）</span>
          </div>
          <assemble-change-set
            :new-assemble-list="item.assembleInfo?.needHandleNewList || []"
            :old-assemble-list="item.assembleInfo?.needHandleOldList || []"
            :assembleInfo="item.assembleInfo"
          />
        </div>
        <div class="box-content">
          <div class="handle-title">
            <span># 部件变更信息</span>
          </div>
          <assemble-change-table :assembleCompareList="item.assembleCompareList" :assembleInfo="item.assembleInfo" />
        </div>
        <div class="box-content" v-if="item.partCompareResList?.length">
          <div class="handle-title">
            <span># 零件变更信息</span>
          </div>
          <machine-part-change-table :partCompareList="item.partCompareResList" />
        </div>
      </el-card>
    </div>
    <artifact-right-info class="right-con" />
  </div>
</template>

<script setup>
import { inject, defineProps } from 'vue'
import artifactChangeTable from '../components/artifact-change-table'
import artifactAreaChangeTable from '../components/artifact-area-change-table'
import artifactRightInfo from '../components/artifact-right-info'
import assembleChangeSet from '../components/assemble-change-set'
import assembleChangeTable from '../components/assemble-change-table'
import machinePartChangeTable from '../components/machine-part-change-table'

defineProps({
  heightStyle: {
    type: String
  }
})

const changeInfo = inject('changeInfo')
</script>

<style lang="scss" scoped>
::-webkit-scrollbar {
  /*滚动条整体样式*/
  width: 4px; /*高宽分别对应横竖滚动条的尺寸*/
  height: 4px;
}
.handle-content {
  display: flex;

  .left-con {
    flex: 1;
    height: 100%;
    overflow: auto;
  }

  .right-con {
    margin-left: 15px;
    width: 300px;
    height: 100%;
  }
}

.artifact-content {
  ::v-deep(.el-card__header) {
    padding: 0;
    display: flex;
    align-items: center;
    width: 100%;
    position: relative;

    .index {
      color: #fff;
      padding: 10px 15px;
      text-align: center;
      font-weight: bold;
      background-color: #0078fc;
      margin-right: 15px;
    }

    .new-sn {
      font-size: 22px;
      font-weight: bold;
      margin-right: 10px;
    }

    .old-sn {
      color: rgb(109, 109, 109);
      font-size: 14px;
    }

    .right-btn {
      position: absolute;
      right: 15px;
      top: 50%;
      transform: translateY(-50%);
      display: flex;
    }

    .circle-button {
      cursor: pointer;
      width: 25px;
      height: 25px;
      border-width: 2px;
      border-style: solid;
      margin-left: 10px;
      border-radius: 50%;
      display: flex;
      align-items: center;
      justify-content: center;
      font-size: 16px;
    }
  }
}

.box-content {
  margin-bottom: 20px;
}
.handle-title {
  font-size: 18px;
  color: rgb(51, 51, 51);
  font-weight: bold;
  white-space: nowrap;
  letter-spacing: 0px;
  word-break: normal;
  margin-bottom: 10px;
}
</style>
