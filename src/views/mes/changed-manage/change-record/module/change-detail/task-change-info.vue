<template>
  <div class="task-content" :style="heightStyle">
    <div class="left-con anchor-container">
      <el-card>
        <template v-for="(c, i) in contentInfo" :key="i">
          <div v-if="c.show" class="type-content">
            <el-divider>
              <span class="title" style="padding: 5px 50px">{{ c.title }}</span>
            </el-divider>
            <template v-for="(s, o) in c.cons" :key="o">
              <div v-if="s.show" class="box-content">
                <div class="task-title anchor-title" :data-anchor-label="s.anchorLabel">
                  <span>{{ s.title }}</span>
                </div>
                <task-change-table v-bind="s.props" />
              </div>
            </template>
          </div>
        </template>
      </el-card>
    </div>
    <el-card class="right-con">
      <anchor-nav ref="anchorNavRef" />
    </el-card>
  </div>
</template>

<script setup>
import { inject, defineProps, computed, watch, ref, nextTick } from 'vue'
import taskChangeTable from '@/components-system/plan/change/task-change-table'
import anchorNav from '@/components-system/common/anchor-nav.vue'

defineProps({
  heightStyle: {
    type: String
  }
})

const anchorNavRef = ref()
const taskInfo = inject('taskInfo')
const contentInfo = computed(() => [
  {
    title: '构件',
    show: Boolean(taskInfo.value?.['artifactList']?.length || taskInfo.value?.['taskOrderArtifactList']?.length || taskInfo.value?.['taskOrderPointWorkArtifactList']?.length),
    cons: [
      {
        title: '# 构件排产变更（任务未下发）',
        anchorLabel: '构件排产变更',
        show: Boolean(taskInfo.value?.['artifactList']?.length),
        props: { compareList: taskInfo.value?.['artifactList'] || [], typeTitle: '构件', showStatus: true }
      },
      {
        title: '# 构件任务变更（【传统线】“任务已下发”且“未生产完成”的构件）',
        anchorLabel: '构件任务变更',
        show: Boolean(taskInfo.value?.['taskOrderArtifactList']?.length),
        props: {
          compareList: taskInfo.value?.['taskOrderArtifactList'] || [],
          typeTitle: '构件',
          showOrderNumber: true,
          showChange: true,
          showProductionQty: true,
          showHandleQty: true
        }
      },
      {
        title: '# 构件点工（【智能线】“任务已下发”且“未生产完成”的构件）',
        anchorLabel: '构件点工(智能线)',
        props: {
          compareList: taskInfo.value['artifactList'],
          typeTitle: '构件',
          showOrderNumber: true,
          showChange: true,
          showHandleQty: true
        }
      },
      {
        title: '# 构件点工（生产完成）',
        anchorLabel: '构件点工(生产完成)',
        show: Boolean(taskInfo.value?.['taskOrderPointWorkArtifactList']?.length),
        props: {
          compareList: taskInfo.value?.['taskOrderPointWorkArtifactList'] || [],
          typeTitle: '构件',
          showChange: true,
          showShippedQty: true,
          showHandleQty: true
        }
      }
    ]
  },
  {
    title: '部件',
    show: Boolean(taskInfo.value?.['assembleList']?.length || taskInfo.value?.['taskOrderAssembleList']?.length || taskInfo.value?.['taskOrderPointWorkAssembleList']?.length),
    cons: [
      {
        title: '# 部件排产变更（“未套料”且“未下发”的部件）',
        anchorLabel: '部件排产变更',
        show: Boolean(taskInfo.value?.['assembleList']?.length),
        props: { compareList: taskInfo.value['assembleList'], typeTitle: '部件', showStatus: true }
      },
      {
        title: '# 部件任务变更（“未套料”且“未生产”的部件）',
        anchorLabel: '部件任务变更',
        show: Boolean(taskInfo.value?.['taskOrderAssembleList']?.length),
        props: { compareList: taskInfo.value['taskOrderAssembleList'], typeTitle: '部件', showHandleQty: true }
      },
      {
        title: '# 部件点工 (“混套”、“生产中”、“生产完成”的部件)',
        anchorLabel: '部件点工',
        show: Boolean(taskInfo.value?.['taskOrderPointWorkAssembleList']?.length),
        props: { compareList: taskInfo.value['taskOrderPointWorkAssembleList'], typeTitle: '部件', showHandleType: true, showHandleQty: true }
      }
    ]
  },
  {
    title: '零件',
    show: Boolean(taskInfo.value?.['machinePartList']?.length),
    cons: [
      {
        title: '# 零件排产变更（“未套料”且 “未下发”的零件）',
        anchorLabel: '零件排产变更',
        show: Boolean(taskInfo.value?.['machinePartList']?.length),
        props: { compareList: taskInfo.value['machinePartList'], typeTitle: '零件', showStatus: true }
      }
    ]
  }
])

watch(
  () => contentInfo,
  () => {
    nextTick(() => {
      anchorNavRef.value?.refreshAnchorList()
    })
  },
  { deep: true, immediate: true }
)
</script>

<style lang="scss" scoped>
::-webkit-scrollbar {
  /*滚动条整体样式*/
  width: 4px; /*高宽分别对应横竖滚动条的尺寸*/
  height: 4px;
}
.task-content {
  display: flex;

  .left-con {
    flex: 1;
    height: 100%;
    overflow: auto;

    .type-content:not(:last-child) {
      margin-bottom: 55px;
    }
  }

  .right-con {
    margin-left: 15px;
    width: 140px;
    height: 100%;

    ::v-deep(.el-card__body) {
      padding: 0px 10px;
    }
  }
}

.box-content {
  margin-bottom: 35px;
}
.task-title {
  font-size: 18px;
  color: rgb(51, 51, 51);
  font-weight: bold;
  white-space: nowrap;
  letter-spacing: 0px;
  word-break: normal;
  margin-bottom: 10px;
}
</style>
